# ============================================================
# LAND USE ANOMALY PIPELINE — PIXEL-LEVEL VERSION (FULL FINAL)
# ============================================================

# ------------------------------------------------------------
# 1) LIBRARIES & SETTINGS
# ------------------------------------------------------------
library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)
library(geojsonio)
library(tibble)
library(tmap)
library(tidygeocoder)
library(mapview)
library(DT)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)

demo_mode <- TRUE
mapviewOptions(fgb = FALSE)
options(mapview.popup = "html")

# ------------------------------------------------------------
# SAFE POPUP HELPER
# ------------------------------------------------------------
popupTableSafe <- function(sf_obj, zcol = NULL) {
  df <- sf::st_drop_geometry(sf_obj)
  if (is.null(zcol)) zcol <- names(df)
  zcol <- intersect(zcol, names(df))
  if (!length(zcol)) return(rep("<i>No attributes</i>", nrow(df)))
  df <- df[, zcol, drop = FALSE]
  df[] <- lapply(df, function(x) htmltools::htmlEscape(as.character(x)))
  vapply(
    seq_len(nrow(df)),
    function(i) {
      row <- df[i, , drop = TRUE]
      paste0(
        "<div style='
          font-family:Arial, sans-serif;
          font-size:13px;
          background-color:#f9f9fb;
          border:1px solid #ddd;
          border-radius:8px;
          box-shadow:0 1px 3px rgba(0,0,0,0.1);
          padding:6px 8px;
          margin:2px;
        '>
          <table style='width:100%; border-collapse:collapse;'>
            ",
        paste0(
          "<tr style='border-bottom:1px solid #eee;'>
                <td style='font-weight:bold; color:#333; padding:3px 6px;'>", names(row), ":</td>
                <td style='padding:3px 6px; color:#555;'>", unname(row), "</td>
              </tr>",
          collapse = ""
        ),
        "
          </table>
        </div>"
      )
    },
    character(1)
  )
}

# ------------------------------------------------------------
# 2) PYTHON + EARTH ENGINE AUTH
# ------------------------------------------------------------
reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python", required = TRUE)

ensure_ee_auth <- function(email, project) {
  tryCatch({
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project, quiet = TRUE)
    message("EE initialized successfully.")
  }, error = function(e) {
    message("Re-authenticating Google Earth Engine...")
    ee_clean_user_credentials()
    googledrive::drive_deauth()
    unlink("~/.config/earthengine", recursive = TRUE, force = TRUE)
    unlink("~/.config/rgee", recursive = TRUE, force = TRUE)
    unlink("~/.R/gargle", recursive = TRUE, force = TRUE)
    ee_Authenticate(drive = TRUE, gcs = FALSE, auth_mode = "notebook")
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project)
  })
}

ensure_ee_auth(
  email = "sds6488292025@students.uonbi.ac.ke",
  project = "ee-sds6488292025"
)

# ------------------------------------------------------------
# 3) LOAD PLANNING ZONES
# ------------------------------------------------------------
zones_path <- "data/test_features.gpkg"
if (!file.exists(zones_path)) stop("Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

if (!("zone_type" %in% names(zones))) {
  stop("The zones layer must contain a 'zone_type' column.")
}

zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4) GET DYNAMIC WORLD DATA (Pixel-level Anomaly Detection)
# ------------------------------------------------------------
# ------------------------------------------------------------
# 4) GET DYNAMIC WORLD DATA (Pixel-level Anomaly Detection)
# ------------------------------------------------------------
dw_collection <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(zones_ee$geometry())$
  filterDate("2024-01-01", as.character(Sys.Date()))$
  select("label")

dw_latest <- dw_collection$sort("system:time_start", FALSE)$first()


# Define class mapping
classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)

# ------------------------------------------------------------
# 5) ZONE-BASED RULES
# ------------------------------------------------------------
# zones_raster <- zones_ee$reduceToImage(
#   properties = list("zone_type"),
#   reducer = ee$Reducer$first()
# )
# 
# allowed_dict <- ee$Dictionary(list(
#   URBAN = 6,          # Built Area
#   AGRICULTURAL = 4,   # Crops
#   FOREST = 1,         # Trees
#   WETLAND = 3,        # Flooded Vegetation
#   INDUSTRIAL = 6      # Built Area
# ))
# 
# allowed_img <- zones_raster$remap(
#   from = list("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
#   to = list(6,4,1,3,6)
# )

# ------------------------------------------------------------
# 5) ZONE-BASED RULES (FIXED: zone_type must be numeric)
# ------------------------------------------------------------

# Create numeric code per zone_type
zone_codes <- zones |>
  st_drop_geometry() |>
  dplyr::distinct(zone_type) |>
  dplyr::mutate(zone_id = dplyr::row_number())

# Join numeric IDs to zones
zones <- zones |>
  left_join(zone_codes, by = "zone_type")

# Convert to Earth Engine
zones_ee <- sf_as_ee(zones)

# Now rasterize using the numeric ID
zones_raster <- zones_ee$reduceToImage(
  properties = list("zone_id"),
  reducer = ee$Reducer$first()
)

# Define allowed mapping by zone_type name
allowed_map_tbl <- tibble::tibble(
  zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
  allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area")
)

# Merge with codes to allow numeric translation
allowed_joined <- allowed_map_tbl |>
  dplyr::mutate(zone_type = toupper(zone_type)) |>
  left_join(zone_codes |> dplyr::mutate(zone_type = toupper(zone_type)), by = "zone_type")

# Build numeric-to-class mapping (dictionary)
allowed_dict <- ee$Dictionary(
  rlang::set_names(
    as.list(allowed_joined$allowed_class),
    as.character(allowed_joined$zone_id)
  )
)

# Map allowed class codes (convert class names to DW numeric labels)
class_to_id <- tibble::tibble(
  class = c("Water","Trees","Grass","Flooded Vegetation","Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice"),
  id = 0:8
)

allowed_joined <- allowed_joined |>
  left_join(class_to_id, by = c("allowed_class" = "class")) |>
  dplyr::filter(!is.na(id))

# Create numeric mapping in GEE
allowed_img <- zones_raster$remap(
  from = as.list(allowed_joined$zone_id),
  to   = as.list(allowed_joined$id)
)






# ------------------------------------------------------------
# 6) ANOMALY MASK CREATION
# ------------------------------------------------------------
anomaly_mask <- dw_latest$neq(allowed_img)
anomaly_img <- dw_latest$updateMask(anomaly_mask)

# Convert anomalous pixels to feature collection
anomalies_fc <- anomaly_img$sample(
  region = zones_ee$geometry(),
  scale = 10,
  projection = "EPSG:4326",
  geometries = TRUE
)

# Convert to R
# anomalies_sf <- ee_as_sf(anomalies_fc, maxFeatures = 20000)

# ------------------------------------------------------------
# SAFE DOWNLOAD HANDLER — handles large exports gracefully
# ------------------------------------------------------------
max_export <- 20000  # safety limit per batch

# Count features in collection
feature_count <- tryCatch({
  anomalies_fc$size()$getInfo()
}, error = function(e) {
  message("⚠️ Could not get feature count — proceeding with default batch size of ", max_export)
  max_export
})

message("📦 Total anomaly pixels available: ", feature_count)

if (feature_count <= max_export) {
  # Small enough — fetch all
  anomalies_sf <- ee_as_sf(anomalies_fc, maxFeatures = max_export)
} else {
  message("⚙️ Exporting in safe batches (this may take a few minutes)...")
  # Sample evenly to reduce the load
  sampling_fraction <- max_export / feature_count
  sampled_fc <- anomalies_fc$randomColumn("rand")$filter(ee$Filter$lt("rand", sampling_fraction))
  
  anomalies_sf <- ee_as_sf(sampled_fc, maxFeatures = max_export)
  message("✅ Downloaded a representative subset of anomalies (", nrow(anomalies_sf), " of ~", feature_count, ")")
}





# ------------------------------------------------------------
# 7) GEOCODING ANOMALIES (PIXEL LEVEL)
# ------------------------------------------------------------
anomalies_sf <- anomalies_sf |>
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  ) |>
  select(-centroid)

if (nrow(anomalies_sf) == 0) {
  message("✅ No anomalies detected — skipping reverse geocoding.")
  anomalies_geocoded <- anomalies_sf |> mutate(address = "No anomalies detected")
} else {
  message("📍 Reverse-geocoding ", nrow(anomalies_sf), " anomaly pixels using OSM...")
  coords_df <- anomalies_sf %>%
    st_drop_geometry() %>%
    select(lon, lat)
  
  geocoded_df <- tryCatch({
    tidygeocoder::reverse_geocode(
      .tbl = coords_df,
      lat = lat,
      long = lon,
      method = "osm",
      address = "address",
      full_results = FALSE
    )
  }, error = function(e) {
    message("Reverse geocoding failed: ", e$message)
    coords_df %>% mutate(address = NA_character_)
  })
  
  if ("long" %in% names(geocoded_df) && !("lon" %in% names(geocoded_df))) {
    geocoded_df <- rename(geocoded_df, lon = long)
  }
  if (!"address" %in% names(geocoded_df)) geocoded_df$address <- NA_character_
  
  anomalies_geocoded <- st_as_sf(geocoded_df, coords = c("lon","lat"), crs = 4326)
  
  if (!all(c("lon","lat") %in% names(anomalies_geocoded))) {
    coords <- st_coordinates(anomalies_geocoded)
    anomalies_geocoded <- anomalies_geocoded %>%
      mutate(lon = coords[,1], lat = coords[,2])
  }
}

# ------------------------------------------------------------
# 8) JOIN BACK WITH ZONES FOR CONTEXT
# ------------------------------------------------------------
anomalies_geocoded <- st_join(anomalies_geocoded, zones, join = st_intersects)

# ------------------------------------------------------------
# 9) PLANNER-FRIENDLY TABLE
# ------------------------------------------------------------
planner_table <- anomalies_geocoded %>%
  st_drop_geometry() %>%
  select(
    Zone = zone_type,
    Longitude = lon,
    Latitude = lat,
    Address = address
  ) %>%
  mutate(
    `Google Maps Link` = paste0(
      "<a href='https://www.google.com/maps?q=", Latitude, ",", Longitude,
      "' target='_blank'>Open in Maps</a>"
    )
  )

# ------------------------------------------------------------
# 10) MAP VISUALIZATION
# ------------------------------------------------------------
base_map <- mapview(
  zones,
  zcol = "zone_type",
  alpha.regions = 0.25,
  layer.name = "Planning Zones",
  map.types = c("OpenStreetMap", "Esri.WorldImagery")
)

if (nrow(anomalies_geocoded) > 0) {
  anom_map <- mapview(
    anomalies_geocoded,
    zcol = "zone_type",
    col.regions = "red",
    cex = 3,
    layer.name = "Anomalous Pixels (2024)",
    popup = popupTableSafe(
      anomalies_geocoded,
      zcol = c("zone_type","address","lon","lat")
    )
  )
  combined_map <- base_map + anom_map
} else {
  combined_map <- base_map
  combined_map@map <- combined_map@map %>%
    leaflet::addControl(html = "<h4 style='color:green;'>No anomalies detected</h4>",
                        position = "topright")
}

if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    data.frame(
      zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      lon = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102),
      address = c("Nairobi CBD","Kilimani","Thika Road","Athi River","Donholm")
    ),
    coords = c("lon","lat"), crs = 4326
  )
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    cex = 9,
    layer.name = "DEMO Fake Anomalies",
    popup = popupTableSafe(demo_pts, zcol = c("zone_type","address"))
  )
  combined_map <- combined_map + demo_map
}

combined_map@map <- combined_map@map %>%
  leaflet.extras::addSearchFeatures(
    targetGroups = c("Planning Zones", "Anomalous Pixels (2024)", "DEMO Fake Anomalies"),
    options = leaflet.extras::searchFeaturesOptions(
      zoom = 12,
      openPopup = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )

# ------------------------------------------------------------
# 11) OUTPUTS & DASHBOARD
# ------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(combined_map@map, "output/anomalies_interactive_map.html", selfcontained = TRUE)

full_dt <- datatable(
  planner_table,
  escape = FALSE,
  extensions = c('Buttons','Scroller'),
  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print'),
                 deferRender = TRUE, scrollY = 400, scroller = TRUE),
  rownames = FALSE
)
htmlwidgets::saveWidget(full_dt, "output/anomalies_planner_table.html", selfcontained = TRUE)

preview_dt <- datatable(head(planner_table, 20),
                        escape = FALSE,
                        options = list(dom = 't', pageLength = 20, scrollY = "300px", paging = FALSE),
                        rownames = FALSE)
htmlwidgets::saveWidget(preview_dt, "output/anomalies_preview_table.html", selfcontained = TRUE)

dashboard_html <- htmltools::tagList(
  tags$head(
    tags$title("Land Use Anomaly Planner Report – 2024"),
    tags$style(HTML("
      body {font-family: Arial, sans-serif; margin: 20px; background: #f8f9fa;}
      h1 {color: #343a40; text-align: center;}
      .footer {text-align: center; margin-top: 50px; color: #6c757d;}
    "))
  ),
  tags$h1("Land Use Anomaly Planner Report – 2024"),
  tags$iframe(src = "anomalies_interactive_map.html", seamless = "seamless", style="width:100%; height:600px; border:none;"),
  tags$iframe(src = "anomalies_preview_table.html", style="width:100%; height:420px; border:none;")
)
htmltools::save_html(dashboard_html, file = "output/planner_dashboard.html")

planner_table_csv <- planner_table %>%
  mutate(`Google Maps Link` = paste0("https://www.google.com/maps?q=", Latitude, ",", Longitude))
write.csv(planner_table_csv, "output/anomalies_planner_table.csv", row.names = FALSE)

st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)

message("\n=== PLANNER DASHBOARD READY ===")
message("Open: output/planner_dashboard.html")
message("Total anomaly pixels: ", nrow(anomalies_geocoded))
if (isTRUE(demo_mode)) message("Demo mode: orange points are fake")
if (interactive()) try(browseURL(file.path(getwd(), "output/planner_dashboard.html")), silent = TRUE)
