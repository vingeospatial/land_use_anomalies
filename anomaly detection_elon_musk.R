# ============================================================
# LAND USE ANOMALY PIPELINE — MULTI-ANOMALY + DASHBOARD (FINAL STABLE)
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
# 3) LOAD PLANNING ZONES (ENSURE zone_type EXISTS)
# ------------------------------------------------------------
zones_path <- "data/national_plan_zone_c.gpkg"
if (!file.exists(zones_path)) stop("Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

# Detect and fix the zone_type column
if (!("zone_type" %in% names(zones))) {
  alt_names <- names(zones)[grepl("zone", names(zones), ignore.case = TRUE)]
  if (length(alt_names) > 0) {
    zones <- zones |> rename(zone_type = !!sym(alt_names[1]))
    message("Renamed ", alt_names[1], " to zone_type")
  } else {
    stop("No column resembling 'zone_type' found. Columns available: ",
         paste(names(zones), collapse = ", "))
  }
}
zones <- zones |> mutate(zone_type = as.character(zone_type))

# Ensure zone_type is not NA and standardize
zones <- zones |> 
  mutate(zone_type = toupper(trimws(ifelse(is.na(zone_type), "UNKNOWN", zone_type))))

# Convert to Earth Engine FeatureCollection
zones_ee <- sf_as_ee(zones)

# Debug: Verify zone_type exists in EE
ee_props <- zones_ee$first()$propertyNames()$getInfo()
message("EE FeatureCollection properties: ", paste(ee_props, collapse = ", "))
if (!("zone_type" %in% ee_props)) {
  message("Warning: zone_type missing in EE FeatureCollection. Adding 'UNKNOWN' as fallback...")
  zones_ee <- zones_ee$map(function(f) {
    f$set("zone_type", f$get("zone_type")$ifNull("UNKNOWN"))
  })
}

# ------------------------------------------------------------
# 4) DYNAMIC WORLD DATA (PIXEL-LEVEL SAMPLING)
# ------------------------------------------------------------
dw_collection <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(zones_ee$geometry())$
  filterDate("2024-01-01", Sys.Date() %>% as.character())$
  select("label")

dw_mode <- dw_collection$reduce(ee$Reducer$mode())

# Sample regions with zone_type property
zonal_stats <- dw_mode$sampleRegions(
  collection = zones_ee,
  properties = list("zone_type"), # Use EE-compatible list
  scale = 10,
  geometries = TRUE
)

# Convert back to sf, handling large datasets
zonal_sf <- tryCatch({
  ee_as_sf(zonal_stats, maxFeatures = 100000)
}, error = function(e) {
  message("Error in ee_as_sf: ", e$message)
  message("Retrying with smaller batch...")
  ee_as_sf(zonal_stats, maxFeatures = 10000)
})

# Debug: Check column names
message("zonal_sf columns: ", paste(names(zonal_sf), collapse = ", "))

# ------------------------------------------------------------
# 5) CLASS LABELS & ANOMALY LOGIC (FIXED)
# ------------------------------------------------------------
classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)

allowed_map <- tibble(
  zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
  allowed_classes = list(
    c("Built Area","Crops","Trees","Grass"),
    c("Crops","Grass","Shrub & Scrub","Bare Ground"),
    c("Trees","Shrub & Scrub","Bare Ground"),
    c("Flooded Vegetation","Water","Grass"),
    c("Built Area","Grass","Trees","Crops")
  )
)

# Standardize and join
zonal_sf <- zonal_sf |>
  mutate(zone_type = toupper(trimws(zone_type))) |>
  rename(mode = any_of(c("mode", "label_mode", "label"))) |> # Handle varying column names
  left_join(classes, by = c("mode" = "id")) |>
  rename(observed_class = class) |>
  left_join(allowed_map, by = "zone_type")

# Safe anomaly detection
zonal_sf <- zonal_sf |>
  mutate(
    anomaly = case_when(
      is.na(observed_class) ~ FALSE,
      is.na(allowed_classes) ~ FALSE,
      !(observed_class %in% unlist(allowed_classes)) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Debug: Anomaly summary
message("Anomaly summary:")
print(table(zonal_sf$anomaly, useNA = "ifany"))

# Debug: Check for Crops in URBAN
urban_crop_mask <- !is.na(zonal_sf$zone_type) &
  !is.na(zonal_sf$observed_class) &
  zonal_sf$zone_type == "URBAN" &
  zonal_sf$observed_class == "Crops"

if (any(urban_crop_mask, na.rm = TRUE)) {
  n <- sum(urban_crop_mask & zonal_sf$anomaly, na.rm = TRUE)
  message("FOUND ", n, " AGRICULTURAL ANOMALIES IN URBAN (CBD) ZONES!")
} else {
  message("No Crops detected in URBAN zones.")
}

# ------------------------------------------------------------
# 6) GEOCODING ANOMALIES
# ------------------------------------------------------------
if (!all(c("lon","lat") %in% names(zonal_sf))) {
  zonal_sf <- zonal_sf |>
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    ) |>
    select(-centroid)
}

anomalies_sf <- zonal_sf |> filter(!is.na(anomaly) & anomaly == TRUE)

if (nrow(anomalies_sf) == 0) {
  message("No anomalies detected — skipping reverse geocoding.")
  anomalies_geocoded <- anomalies_sf |> mutate(address = "No anomalies detected")
} else {
  message("Reverse-geocoding ", nrow(anomalies_sf), " anomalies using OSM...")
  coords_df <- anomalies_sf %>%
    st_drop_geometry() %>%
    select(zone_type, observed_class, allowed_classes, lon, lat)
  
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
  
  anomalies_geocoded <- st_as_sf(geocoded_df, coords = c("lon", "lat"), crs = 4326)
  
  if (!all(c("lon","lat") %in% names(anomalies_geocoded))) {
    coords <- st_coordinates(anomalies_geocoded)
    anomalies_geocoded <- anomalies_geocoded %>%
      mutate(lon = coords[,1], lat = coords[,2])
  }
}

zonal_sf <- zonal_sf %>%
  left_join(
    anomalies_geocoded %>% st_drop_geometry() %>% select(lon, lat, address),
    by = c("lon", "lat")
  )

# ------------------------------------------------------------
# 7) PLANNER TABLE
# ------------------------------------------------------------
planner_table <- anomalies_geocoded %>%
  st_drop_geometry() %>%
  select(
    Zone = zone_type,
    `Observed Class` = observed_class,
    `Allowed Classes` = allowed_classes,
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
# 8) MAP & DASHBOARD
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
    zcol = NULL,
    col.regions = "red",
    cex = 5,
    layer.name = "Anomalies (2024)",
    popup = popupTableSafe(
      anomalies_geocoded,
      zcol = c("zone_type","observed_class","allowed_classes","address","lon","lat")
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
      observed_class = c("Crops","Built Area","Bare Ground","Crops","Trees"),
      allowed_classes = I(list(rep(NA,5))),
      lon = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102),
      address = c("Nairobi CBD","Kilimani","Thika Road","Athi River","Donholm")
    ),
    coords = c("lon","lat"), crs = 4326
  ) %>% mutate(anomaly = TRUE)
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    cex = 9,
    layer.name = "DEMO Fake Anomalies",
    popup = popupTableSafe(demo_pts, zcol = c("zone_type","observed_class","address"))
  )
  combined_map <- combined_map + demo_map
}

combined_map@map <- combined_map@map %>%
  leaflet.extras::addSearchFeatures(
    targetGroups = c("Planning Zones", "Anomalies (2024)", "DEMO Fake Anomalies"),
    options = leaflet.extras::searchFeaturesOptions(
      zoom = 12,
      openPopup = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )

# ------------------------------------------------------------
# 9) SAVE OUTPUTS
# ------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(combined_map@map, "output/anomalies_interactive_map.html", selfcontained = TRUE)

full_dt <- datatable(
  planner_table,
  escape = FALSE,
  extensions = c('Buttons','Scroller'),
  options = list(
    dom = 'Bfrtip',
    buttons = c('copy','csv','excel','pdf','print'),
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ),
  rownames = FALSE
)
htmlwidgets::saveWidget(full_dt, "output/anomalies_planner_table.html", selfcontained = TRUE)

preview_dt <- datatable(
  head(planner_table, 20),
  escape = FALSE,
  options = list(dom = 't', pageLength = 20, scrollY = "300px", paging = FALSE),
  rownames = FALSE
)
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
  tags$iframe(src = "anomalies_interactive_map.html",
              seamless = "seamless",
              style="width:100%; height:600px; border:none;"),
  tags$iframe(src = "anomalies_preview_table.html",
              style="width:100%; height:420px; border:none;")
)
htmltools::save_html(dashboard_html, file = "output/planner_dashboard.html")

# CSV
planner_table_csv <- planner_table %>%
  mutate(`Google Maps Link` = paste0("https://www.google.com/maps?q=", Latitude, ",", Longitude))
write.csv(planner_table_csv, "output/anomalies_planner_table.csv", row.names = FALSE)

# GPKG
st_write(zonal_sf, "output/land_use_anomalies_full.gpkg", delete_dsn = TRUE, quiet = TRUE)
if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)
} else {
  empty_sf <- st_sf(address = character(), geometry = st_sfc(crs = 4326))
  st_write(empty_sf, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)
}

# ------------------------------------------------------------
# 10) FINAL MESSAGE
# ------------------------------------------------------------
message("\n=== PLANNER DASHBOARD READY ===")
message("Open: output/planner_dashboard.html")
message("Total REAL anomalies: ", nrow(anomalies_geocoded))
if (isTRUE(demo_mode)) message("Demo mode: orange points are fake")
if (interactive()) {
  try(browseURL(file.path(getwd(), "output/planner_dashboard.html")), silent = TRUE)
}