# ============================================================
# LAND USE ANOMALY PIPELINE — PIXEL-LEVEL VERSION (FINAL + PATCHED)
# ============================================================
# Author: Urban Planner + R + Google Earth Engine
# Date: 2025-11-01
# Purpose: Detect land use violations at 10m resolution using Dynamic World
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
library(progress)

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
        "<div style='font-family:Arial,sans-serif;font-size:13px;background:#f9f9fb;
        border:1px solid #ddd;border-radius:8px;padding:6px 8px;margin:2px;
        box-shadow:0 1px 3px rgba(0,0,0,0.1);'>
          <table style='width:100%;border-collapse:collapse;'>",
        paste0(
          "<tr style='border-bottom:1px solid #eee;'>
            <td style='font-weight:bold;color:#333;padding:3px 6px;'>", names(row), ":</td>
            <td style='padding:3px 6px;color:#555;'>", unname(row), "</td>
          </tr>",
          collapse = ""
        ),
        "</table></div>"
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
    message("✅ EE initialized successfully.")
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
zones_path <- "data/one_feature.gpkg"
if (!file.exists(zones_path)) stop("❌ Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

if (!"zone_type" %in% names(zones)) {
  stop("The zones layer must contain a 'zone_type' column.")
}

zones <- zones |> mutate(zone_type = toupper(zone_type))
zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4) DYNAMIC WORLD DATA (Latest per-pixel mosaic)
# ------------------------------------------------------------
dw_collection <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(zones_ee$geometry())$
  filterDate("2024-01-01", as.character(Sys.Date()))$
  select("label")

# Newest-first mosaic (latest observation per pixel)
dw_latest <- dw_collection$sort("system:time_start", FALSE)$mosaic()

# Class mapping
classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)

# ------------------------------------------------------------
# 5) ZONE-BASED RULES
# ------------------------------------------------------------
zone_codes <- zones |>
  st_drop_geometry() |>
  distinct(zone_type) |>
  mutate(zone_id = row_number())

zones <- zones |> left_join(zone_codes, by = "zone_type")
zones_ee <- sf_as_ee(zones)

zones_raster <- zones_ee$reduceToImage(
  properties = list("zone_id"),
  reducer = ee$Reducer$first()
)

allowed_map_tbl <- tibble(
  zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
  allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area")
) |> mutate(zone_type = toupper(zone_type))

allowed_joined <- allowed_map_tbl |>
  left_join(zone_codes, by = "zone_type") |>
  left_join(classes, by = c("allowed_class" = "class")) |>
  filter(!is.na(id), !is.na(zone_id))

missing <- setdiff(unique(zones$zone_type), allowed_map_tbl$zone_type)
if (length(missing)) message("⚠️ No rules for zone_type(s): ", paste(missing, collapse = ", "))

allowed_img <- zones_raster$remap(
  from = ee$List(as.list(allowed_joined$zone_id)),
  to   = ee$List(as.list(allowed_joined$id))
)$unmask(999)

# ------------------------------------------------------------
# 6) ANOMALY DETECTION + PIXEL FOOTPRINTS
# ------------------------------------------------------------
# Smooth Dynamic World labels: most common class within ~10 pixels (~100 m)
dw_smoothed <- dw_latest$focal_mode(
  radius = 100,  # meters
  units = "meters"
)

# Now compare the smoothed map against allowed class per zone
anomaly_mask <- dw_smoothed$neq(allowed_img)$And(allowed_img$neq(999))

# Mask out valid areas, keep only anomalies
anomaly_img <- dw_smoothed$updateMask(anomaly_mask)


# anomaly_mask <- dw_latest$neq(allowed_img)$And(allowed_img$neq(999))
# anomaly_img <- dw_latest$updateMask(anomaly_mask)

# Sample anomalous pixels with 10m footprint (as points → buffer later)
anomalies_fc <- anomaly_img$stratifiedSample(
  region = zones_ee$geometry(),
  scale = 10,
  classBand = "label",
  numPoints = 5000,
  geometries = TRUE,
  seed = 42
)

# ------------------------------------------------------------
# 7) SAFE DOWNLOAD
# ------------------------------------------------------------
max_export <- 20000
message("⬇️ Downloading anomaly pixels from Earth Engine...")

anomalies_sf <- tryCatch({
  ee_as_sf(anomalies_fc, maxFeatures = max_export, via = "drive")
}, error = function(e) {
  message("Too many features. Sampling 10% randomly...")
  sampled_fc <- anomalies_fc$randomColumn("rand")$filter(ee$Filter$lt("rand", 0.1))
  ee_as_sf(sampled_fc, maxFeatures = max_export, via = "drive")
})

# ------------------------------------------------------------
# 8) POST-PROCESS & GEOCODE
# ------------------------------------------------------------
if (nrow(anomalies_sf) == 0) {
  message("✅ No anomalies detected in the area.")
  anomalies_geocoded <- st_sf(
    zone_type = character(),
    anomaly_class = character(),
    address = character(),
    lon = numeric(),
    lat = numeric(),
    geometry = st_sfc(crs = 4326)
  )
} else {
  message("📍 Preparing anomaly geometries...")
  metric_crs <- 3857
  
  anomalies_sf <- anomalies_sf %>%
    st_transform(metric_crs) %>%
    st_buffer(dist = 5) %>%
    st_transform(4326) %>%
    left_join(classes, by = c("label" = "id")) %>%
    rename(anomaly_class = class) %>%
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[,1],
      lat = st_coordinates(centroid)[,2]
    ) %>%
    select(-centroid)
  
  message("🗺️ Reverse geocoding ", nrow(anomalies_sf), " anomaly pixels...")
  coords_df <- anomalies_sf %>% st_drop_geometry() %>% select(lon, lat)
  pb <- progress_bar$new(format = "[:bar] :percent :elapsed | :current/:total", total = nrow(coords_df))
  
  geocoded_list <- lapply(seq_len(nrow(coords_df)), function(i) {
    pb$tick()
    row <- coords_df[i, ]
    Sys.sleep(0.2)
    res <- tryCatch({
      tidygeocoder::reverse_geocode(
        .tbl = tibble(lat = row$lat, lon = row$lon),
        lat = lat, long = lon,
        method = "osm", address = "address", full_results = FALSE, quiet = TRUE
      )
    }, error = function(e) tibble(address = NA_character_))
    if (!"address" %in% names(res)) res$address <- NA_character_
    res$address[is.na(res$address)] <- "Address not found"
    res
  })
  
  geocoded_df <- bind_rows(geocoded_list)
  
  anomalies_geocoded <- anomalies_sf %>%
    bind_cols(geocoded_df %>% select(address)) %>%
    st_join(zones %>% select(zone_type), join = st_intersects, left = TRUE)
}

# ------------------------------------------------------------
# 9) PLANNER TABLES
# ------------------------------------------------------------
planner_table <- anomalies_geocoded %>%
  st_drop_geometry() %>%
  select(
    Zone = zone_type,
    `Anomaly Type` = anomaly_class,
    Address = address,
    Longitude = lon,
    Latitude = lat
  ) %>%
  mutate(
    `Google Maps Link` = paste0(
      "<a href='https://www.google.com/maps?q=", Latitude, ",", Longitude,
      "' target='_blank'>Open in Maps</a>"
    )
  )

summary_by_zone <- planner_table %>%
  count(Zone, name = "Anomaly_Count") %>%
  arrange(desc(Anomaly_Count))

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
    zcol = "anomaly_class",
    col.regions = "red",
    cex = 4,
    alpha = 0.8,
    layer.name = "Anomalous Pixels (2024)",
    popup = popupTableSafe(
      anomalies_geocoded,
      zcol = c("zone_type", "anomaly_class", "address", "lon", "lat")
    )
  )
  combined_map <- base_map + anom_map
} else {
  combined_map <- base_map
  combined_map@map <- combined_map@map %>%
    leaflet::addControl(
      html = "<h4 style='color:green;margin:10px;'>No land use anomalies detected</h4>",
      position = "topright"
    )
}

if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    data.frame(
      zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      anomaly_class = c("Crops","Built Area","Bare Ground","Trees","Water"),
      address = c("Nairobi CBD","Kilimani Farm","Thika Forest","Athi River","Donholm"),
      lon = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102)
    ),
    coords = c("lon","lat"), crs = 4326
  ) %>% st_buffer(dist = 0.0001)
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    cex = 8,
    layer.name = "DEMO Fake Anomalies",
    popup = popupTableSafe(demo_pts, zcol = c("zone_type","anomaly_class","address"))
  )
  combined_map <- combined_map + demo_map
}

combined_map@map <- combined_map@map %>%
  leaflet.extras::addSearchFeatures(
    targetGroups = c("Planning Zones", "Anomalous Pixels (2024)", "DEMO Fake Anomalies"),
    options = leaflet.extras::searchFeaturesOptions(
      zoom = 15, openPopup = TRUE, hideMarkerOnCollapse = TRUE
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

preview_dt <- datatable(
  head(planner_table, 20),
  escape = FALSE,
  options = list(dom = 't', pageLength = 20, scrollY = "300px", paging = FALSE),
  rownames = FALSE
)
htmlwidgets::saveWidget(preview_dt, "output/anomalies_preview_table.html", selfcontained = TRUE)

summary_dt <- datatable(
  summary_by_zone,
  options = list(dom = 't', pageLength = 10),
  rownames = FALSE,
  caption = "Anomalies by Planning Zone"
)
htmlwidgets::saveWidget(summary_dt, "output/anomaly_summary_table.html", selfcontained = TRUE)

report_year <- format(Sys.Date(), "%Y")

dashboard_html <- htmltools::tagList(
  tags$head(
    tags$title(paste0("Land Use Anomaly Report – ", report_year)),
    tags$style(HTML("
      body {font-family: 'Segoe UI', Arial, sans-serif; margin: 20px; background: #f8f9fa; color: #333;}
      h1, h2 {color: #2c3e50; text-align: center;}
      .section {margin: 30px 0; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);}
      .footer {text-align: center; margin-top: 50px; color: #6c757d; font-size: 0.9em;}
      iframe {border: 1px solid #ddd; border-radius: 8px;}
    "))
  ),
  tags$h1(paste0("Land Use Anomaly Detection Report – ", report_year)),
  tags$div(class = "section",
           tags$h2("Interactive Map"),
           tags$iframe(src = "anomalies_interactive_map.html", width = "100%", height = "600", seamless = "seamless")
  ),
  tags$div(class = "section",
           tags$h2("Anomaly Summary by Zone"),
           tags$iframe(src = "anomaly_summary_table.html", width = "100%", height = "250", seamless = "seamless")
  ),
  tags$div(class = "section",
           tags$h2("Top 20 Anomalies (Planner Table)"),
           tags$iframe(src = "anomalies_preview_table.html", width = "100%", height = "450", seamless = "seamless")
  ),
  tags$div(class = "footer",
           tags$p("Generated on ", Sys.Date(), " | Powered by Google Earth Engine + R + OpenStreetMap"),
           tags$p("Total anomaly pixels: ", nrow(anomalies_geocoded),
                  ifelse(demo_mode, " | DEMO MODE ACTIVE (orange points are fake)", ""))
  )
)

htmltools::save_html(dashboard_html, file = "output/planner_dashboard.html")

planner_table_csv <- planner_table %>%
  mutate(`Google Maps Link` = paste0("https://www.google.com/maps?q=", Latitude, ",", Longitude))
write.csv(planner_table_csv, "output/anomalies_planner_table.csv", row.names = FALSE)
write.csv(summary_by_zone, "output/anomaly_summary_by_zone.csv", row.names = FALSE)

st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ------------------------------------------------------------
# 12) FINAL MESSAGE
# ------------------------------------------------------------
message("\n=== LAND USE ANOMALY PIPELINE: SUCCESS ===")
message("   Anomalies detected: ", nrow(anomalies_geocoded))
message("   Dashboard: ", file.path(getwd(), "output/planner_dashboard.html"))
message("   GIS layer: output/land_use_anomalies_geocoded.gpkg")
message("   CSV table: output/anomalies_planner_table.csv")
if (isTRUE(demo_mode)) message("   DEMO MODE: Orange points are fake")
if (interactive()) try(browseURL(file.path(getwd(), "output/planner_dashboard.html")), silent = TRUE)
