# ============================================================
# LAND USE ANOMALY PIPELINE — PIXEL + POLYGON BOUNDARIES (50m SQUARES)
# ============================================================
# Author: Urban Planner + R + Google Earth Engine
# Date: 2025-11-01
# Purpose: Detect land use violations (Dynamic World) and visualize as 50m tiles
# Features:
# - Pixel-level sampling + reverse geocoding
# - Polygon boundary outlines
# - Probability-based class correction (Crops → Built if Built>=0.40 and >= Crops)
# - Smoothing with focal_mode
# - 50m grid-aligned anomaly tiles (squares)
# - Safe exports, progress bars, fallbacks
# ============================================================

# ------------------------------------------------------------
# 1) LIBRARIES & SETTINGS
# ------------------------------------------------------------
library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)
library(tibble)
library(tidygeocoder)
library(mapview)
library(DT)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(progress)
library(purrr)

demo_mode <- TRUE
mapviewOptions(fgb = FALSE)
options(mapview.popup = "html")

# ------------------------------------------------------------
# SAFE POPUP HELPER (XSS-safe)
# ------------------------------------------------------------
popupTableSafe <- function(sf_obj, zcol = NULL) {
  df <- st_drop_geometry(sf_obj)
  if (is.null(zcol)) zcol <- names(df)
  zcol <- intersect(zcol, names(df))
  if (!length(zcol)) return(rep("<i>No attributes</i>", nrow(df)))
  df <- df[, zcol, drop = FALSE]
  df[] <- lapply(df, function(x) htmlEscape(as.character(x)))
  vapply(seq_len(nrow(df)), function(i) {
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
        </tr>", collapse = ""
      ),
      "</table></div>"
    )
  }, character(1))
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
zones_path <- "data/one_feature.gpkg"
if (!file.exists(zones_path)) stop("Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

if (!"zone_type" %in% names(zones)) {
  stop("The zones layer must contain a 'zone_type' column.")
}

zones <- zones |> mutate(zone_type = toupper(zone_type))
zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4) DYNAMIC WORLD + PROBABILITY CORRECTION
# ------------------------------------------------------------
dw_collection <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(zones_ee$geometry())$
  filterDate("2024-01-01", as.character(Sys.Date()))

# Latest label mosaic
dw_latest_label <- dw_collection$select("label")$
  sort("system:time_start", FALSE)$mosaic()

# Latest probability bands
dw_probs <- dw_collection$
  select(c("water","trees","grass","flooded_vegetation","crops",
           "shrub_and_scrub","built","bare","snow_and_ice"))$
  sort("system:time_start", FALSE)$mosaic()

# Fix common misclassification: Crops → Built if built_prob >= crops_prob AND built_prob >= 0.40
CROPS_ID <- 4L
BUILT_ID <- 6L
built_prob <- dw_probs$select("built")
crops_prob <- dw_probs$select("crops")

crop_but_really_built <- dw_latest_label$eq(CROPS_ID)$
  And(built_prob$gte(crops_prob))$
  And(built_prob$gte(0.40))

dw_fixed_label <- dw_latest_label$where(crop_but_really_built, BUILT_ID)

# Class mapping
classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)

# ------------------------------------------------------------
# 5) ZONE RULES + RASTERIZATION
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

missing_zones <- setdiff(unique(zones$zone_type), allowed_map_tbl$zone_type)
if (length(missing_zones)) {
  message("No rules defined for zone(s): ", paste(missing_zones, collapse = ", "))
}

allowed_img <- zones_raster$remap(
  from = ee$List(as.list(allowed_joined$zone_id)),
  to = ee$List(as.list(allowed_joined$id))
)$unmask(999)  # 999 = no rule

# ------------------------------------------------------------
# 6) SMOOTHING + ANOMALY MASK + POLYGON BOUNDARIES
# ------------------------------------------------------------
dw_smoothed <- dw_fixed_label$focal_mode(radius = 1000, units = "meters")

anomaly_mask <- dw_smoothed$neq(allowed_img)$And(allowed_img$neq(999))
anomaly_img <- dw_smoothed$updateMask(anomaly_mask)

# Vectorize anomaly boundaries
anomaly_vectors <- anomaly_mask$selfMask()$reduceToVectors(
  geometry = zones_ee$geometry(),
  scale = 100,
  geometryType = 'polygon',
  eightConnected = TRUE,
  labelProperty = 'anomaly',
  bestEffort = TRUE,
  tileScale = 4
)

# Download polygon boundaries
message("Downloading anomaly boundaries...")
anomaly_poly_sf <- tryCatch({
  ee_as_sf(anomaly_vectors, maxFeatures = 20000, via = "drive")
}, error = function(e) {
  message("Too many polygons. Sampling 50%...")
  sampled <- anomaly_vectors$randomColumn("r")$filter(ee$Filter$lt("r", 0.5))
  ee_as_sf(sampled, maxFeatures = 10000, via = "drive")
}) %>%
  st_make_valid() %>%
  st_transform(4326)

# ------------------------------------------------------------
# 7) DOWNLOAD PIXELS
# ------------------------------------------------------------
max_export <- 20000
message("Downloading anomaly pixel samples...")
anomalies_sf <- tryCatch({
  ee_as_sf(
    anomaly_img$stratifiedSample(
      region = zones_ee$geometry(),
      scale = 10,
      numPoints = 5000,
      geometries = TRUE,
      seed = 42
    ),
    maxFeatures = max_export, via = "drive"
  )
}, error = function(e) {
  message("Too many points. Sampling 10%...")
  sampled_fc <- anomaly_img$stratifiedSample(
    region = zones_ee$geometry(),
    scale = 10,
    numPoints = 500,
    geometries = TRUE,
    seed = 42
  )
  ee_as_sf(sampled_fc, maxFeatures = max_export, via = "drive")
})

# ------------------------------------------------------------
# 8) GEOCODE + 50m GRID-ALIGNED SQUARE FOOTPRINTS
# ------------------------------------------------------------
if (nrow(anomalies_sf) == 0) {
  message("No anomalies detected.")
  anomalies_geocoded <- st_sf(
    zone_type = character(), anomaly_class = character(),
    address = character(), lon = numeric(), lat = numeric(),
    geometry = st_sfc(crs = 4326)
  )
} else {
  message("Generating 50m x 50m square footprints for anomalies (grid-aligned)...")
  metric_crs <- 3857
  
  # Transform to metric CRS and compute centroid & coords
  anomalies_sf <- anomalies_sf %>%
    st_transform(metric_crs) %>%
    mutate(centroid = st_centroid(geometry))
  
  # Extract centroid coordinates
  cent_xy <- st_coordinates(anomalies_sf$centroid)
  anomalies_sf$cx <- cent_xy[, 1]
  anomalies_sf$cy <- cent_xy[, 2]
  
  # Snap each centroid to the lower-left corner of a 50 m grid cell
  tile <- function(x0, y0, size = 50) {
    # Build a single square polygon from (x0,y0) to (x0+size,y0+size)
    st_polygon(list(matrix(
      c(x0, y0,
        x0 + size, y0,
        x0 + size, y0 + size,
        x0, y0 + size,
        x0, y0), ncol = 2, byrow = TRUE
    )))
  }
  
  # Compute snapped origins and build polygons
  x_ll <- floor(anomalies_sf$cx / 50) * 50
  y_ll <- floor(anomalies_sf$cy / 50) * 50
  poly_list <- map2(x_ll, y_ll, ~ tile(.x, .y, 50))
  
  # Replace geometry with the 50m tiles
  anomalies_sf$geometry <- st_sfc(poly_list, crs = metric_crs)
  
  # Join classes, compute lon/lat from tile centroids, back to WGS84
  anomalies_sf <- anomalies_sf %>%
    st_transform(4326) %>%
    left_join(classes, by = c("label" = "id")) %>%
    rename(anomaly_class = class) %>%
    mutate(
      lon = st_coordinates(st_centroid(geometry))[, 1],
      lat = st_coordinates(st_centroid(geometry))[, 2]
    ) %>%
    select(-centroid, -cx, -cy)
  
  # Reverse geocode
  message("Reverse geocoding ", nrow(anomalies_sf), " tiles...")
  coords_df <- anomalies_sf %>% st_drop_geometry() %>% select(lon, lat)
  pb <- progress_bar$new(format = "[:bar] :percent :elapsed | :current/:total", total = nrow(coords_df))
  
  geocoded_list <- lapply(seq_len(nrow(coords_df)), function(i) {
    pb$tick()
    row <- coords_df[i, ]
    Sys.sleep(0.15)  # be polite to OSM
    res <- tryCatch({
      tidygeocoder::reverse_geocode(
        tibble(lat = row$lat, lon = row$lon),
        lat = lat, long = lon,
        method = "osm", address = "address", full_results = FALSE, quiet = TRUE
      )
    }, error = function(e) tibble(address = NA_character_))
    res$address <- ifelse(is.na(res$address) | res$address == "", "Address not found", res$address)
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
# 10) INTERACTIVE MAP (Boundaries + 50m Tiles)
# ------------------------------------------------------------
base_map <- mapview(
  zones,
  zcol = "zone_type",
  alpha.regions = 0.25,
  layer.name = "Planning Zones",
  map.types = c("OpenStreetMap", "Esri.WorldImagery")
)

if (nrow(anomalies_geocoded) > 0) {
  # 50m anomaly tiles (polygons)
  anom_tiles_map <- mapview(
    anomalies_geocoded,
    zcol = "anomaly_class",
    col.regions = "#FF0000",
    alpha = 0.6,
    layer.name = "Anomaly Tiles (50m)",
    popup = popupTableSafe(anomalies_geocoded,
                           zcol = c("zone_type", "anomaly_class", "address", "lon", "lat"))
  )
  
  # Anomaly boundaries (vectorized mask)
  anom_poly_map <- mapview(
    anomaly_poly_sf,
    layer.name = "Anomaly Boundaries",
    color = "red",
    lwd = 2,
    alpha.regions = 0
  )
  
  combined_map <- base_map + anom_poly_map + anom_tiles_map
} else {
  combined_map <- base_map
  combined_map@map <- combined_map@map %>%
    leaflet::addControl(
      html = "<h4 style='color:green;margin:10px;'>No land use anomalies detected</h4>",
      position = "topright"
    )
}

# Demo layer (optional)
if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    tibble(
      zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      anomaly_class = c("Crops","Built Area","Bare Ground","Trees","Water"),
      address = c("Nairobi CBD","Kilimani","Thika Road","Athi River","Donholm"),
      lon = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102)
    ),
    coords = c("lon", "lat"), crs = 4326
  ) %>%
    st_transform(3857) %>%
    # turn demo points into 50m tiles too
    mutate(
      cx = st_coordinates(.)[,1],
      cy = st_coordinates(.)[,2],
      x_ll = floor(cx/50)*50,
      y_ll = floor(cy/50)*50
    ) %>%
    mutate(geometry = st_sfc(pmap(list(x_ll, y_ll), ~ st_polygon(list(matrix(
      c(..1, ..2,
        ..1 + 50, ..2,
        ..1 + 50, ..2 + 50,
        ..1, ..2 + 50,
        ..1, ..2), ncol = 2, byrow = TRUE
    )))), crs = 3857)) %>%
    st_transform(4326) %>%
    select(-cx,-cy,-x_ll,-y_ll)
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    alpha = 0.6,
    layer.name = "DEMO Fake Anomalies (50m)",
    popup = popupTableSafe(demo_pts, zcol = c("zone_type","anomaly_class","address"))
  )
  combined_map <- combined_map + demo_map
}

combined_map@map <- combined_map@map %>%
  leaflet.extras::addSearchFeatures(
    targetGroups = c("Planning Zones","Anomaly Boundaries","Anomaly Tiles (50m)","DEMO Fake Anomalies (50m)"),
    options = leaflet.extras::searchFeaturesOptions(zoom = 15, openPopup = TRUE)
  )

# ------------------------------------------------------------
# 11) OUTPUTS & DASHBOARD
# ------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Save map
saveWidget(combined_map@map, "output/anomalies_interactive_map.html", selfcontained = TRUE)

# Save tables
full_dt <- datatable(
  planner_table, escape = FALSE,
  extensions = c('Buttons','Scroller'),
  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel'), scrollY = 400, scroller = TRUE),
  rownames = FALSE
)
saveWidget(full_dt, "output/anomalies_planner_table.html", selfcontained = TRUE)

preview_dt <- datatable(head(planner_table, 20), escape = FALSE,
                        options = list(dom = 't', pageLength = 20))
saveWidget(preview_dt, "output/anomalies_preview_table.html", selfcontained = TRUE)

summary_dt <- datatable(summary_by_zone, caption = "Anomalies by Zone")
saveWidget(summary_dt, "output/anomaly_summary_table.html", selfcontained = TRUE)

# CSV exports
write.csv(planner_table %>% mutate(`Google Maps Link` = paste0("https://www.google.com/maps?q=", Latitude, ",", Longitude)),
          "output/anomalies_planner_table.csv", row.names = FALSE)
write.csv(summary_by_zone, "output/anomalies_summary_by_zone.csv", row.names = FALSE)

# GeoPackage
st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(anomaly_poly_sf, "output/anomaly_boundaries.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ------------------------------------------------------------
# 12) DASHBOARD HTML
# ------------------------------------------------------------
dashboard_html <- tagList(
  tags$head(
    tags$title("Land Use Anomaly Report – 2024"),
    tags$style(HTML("
      body {font-family: 'Segoe UI', Arial; margin: 20px; background: #f8f9fa;}
      h1, h2 {color: #2c3e50; text-align: center;}
      .section {margin: 30px 0; padding: 20px; background: white; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);}
      iframe {border: 1px solid #ddd; border-radius: 8px; width: 100%;}
      .footer {text-align: center; margin: 50px 0; color: #6c757d; font-size: 0.9em;}
    "))
  ),
  tags$h1("Land Use Anomaly Detection Report – 2024"),
  tags$div(class = "section",
           tags$h2("Interactive Map (Boundaries + 50m Tiles)"),
           tags$iframe(src = "anomalies_interactive_map.html", height = "600", seamless = "seamless")
  ),
  tags$div(class = "section",
           tags$h2("Top 20 Anomalies"),
           tags$iframe(src = "anomalies_preview_table.html", height = "450", seamless = "seamless")
  ),
  tags$div(class = "footer",
           tags$p(paste0("Generated: ", Sys.Date(), " | Total anomalies: ", nrow(anomalies_geocoded))),
           tags$p(ifelse(demo_mode, "DEMO MODE ACTIVE", ""))
  )
)

save_html(dashboard_html, "output/planner_dashboard.html")

# ------------------------------------------------------------
# 13) FINAL MESSAGE
# ------------------------------------------------------------
message("\nLAND USE ANOMALY PIPELINE: SUCCESS")
message("   Anomalies detected (tiles): ", nrow(anomalies_geocoded))
message("   Dashboard: output/planner_dashboard.html")
message("   Map: output/anomalies_interactive_map.html")
message("   Boundaries: output/anomaly_boundaries.gpkg")
message("   GIS layer: output/land_use_anomalies_geocoded.gpkg")
if (isTRUE(demo_mode)) message("   DEMO MODE: Orange tiles are fake")
if (interactive()) browseURL("output/planner_dashboard.html")
