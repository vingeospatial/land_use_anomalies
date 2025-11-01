# ============================================================
# LAND USE ANOMALY PIPELINE — PRODUCTION READY
# ============================================================
# ------------------------------------------------------------
# 1 LIBRARIES & SETTINGS
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
library(webshot)

# ============================================================
# SAFE POPUP HELPER (replaces mapview::popupTable)
# ============================================================
popupTableSafe <- function(sf_obj, zcol = NULL) {
  # Drop geometry
  df <- sf::st_drop_geometry(sf_obj)
  if (is.null(zcol)) zcol <- names(df)
  df <- df[, zcol, drop = FALSE]
  
  # Escape HTML
  df[] <- lapply(df, function(x) htmltools::htmlEscape(as.character(x)))
  
  # Build popup content per feature
  popups <- apply(df, 1, function(row) {
    paste0("<table style='font-size:12px;'>",
           paste0("<tr><td><b>", names(row), ":</b></td><td>", row, "</td></tr>",
                  collapse = ""),
           "</table>")
  })
  return(popups)
}

demo_mode <- TRUE
mapviewOptions(fgb = FALSE) # ensures Leaflet rendering for wide compatibility
options(mapview.popup = "auto")  # Optional: improves popup handling

# ------------------------------------------------------------
# 2 PYTHON + EARTH ENGINE AUTHENTICATION
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
# 3 LOAD PLANNING ZONES
# ------------------------------------------------------------
zones <- st_read("data/national_plan_zone.gpkg") %>%
  st_make_valid() %>%
  st_transform(4326)

zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4 GET DYNAMIC WORLD DATA
# ------------------------------------------------------------
dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01", "2024-12-31")$
  filterBounds(zones_ee$geometry())$
  median()$
  select("label")

zonal_stats <- dw$reduceRegions(
  collection = zones_ee,
  reducer = ee$Reducer$mode(),
  scale = 10,
  crs = "EPSG:4326"
)

zonal_sf <- ee_as_sf(zonal_stats, maxFeatures = 10000)

# ------------------------------------------------------------
# 5 STANDARDIZE MODE COLUMN NAME
# ------------------------------------------------------------
pick_mode_col <- function(nm) {
  cands <- c("mode", ".mode", "label_mode", "label")
  m <- intersect(cands, nm)
  if (length(m) == 0) stop("Couldn't find mode column in EE output. Found: ", paste(nm, collapse = ", "))
  m[1]
}

mode_col <- pick_mode_col(names(zonal_sf))
zonal_sf <- zonal_sf %>%
  rename(mode = !!rlang::sym(mode_col))

# ------------------------------------------------------------
# 6 CLASS LABELS & ANOMALY LOGIC
# ------------------------------------------------------------
classes <- tibble(
  id = 0:8,
  class = c("Water", "Trees", "Grass", "Flooded Vegetation",
            "Crops", "Shrub & Scrub", "Built Area", "Bare Ground", "Snow/Ice")
)

zonal_sf <- zonal_sf %>%
  left_join(classes, by = c("mode" = "id")) %>%
  rename(observed_class = class)

allowed_map <- tibble(
  zone_type = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
  allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area")
)

before_n_na <- sum(is.na(zonal_sf$zone_type))
zonal_sf <- zonal_sf %>% left_join(allowed_map, by = "zone_type")
after_n_na <- sum(is.na(zonal_sf$allowed_class))

if (after_n_na > 0) {
  warn_levels <- setdiff(unique(zonal_sf$zone_type), allowed_map$zone_type)
  message("Warning: ", after_n_na, " zones not matched in allowed_map. Missing: ", paste(warn_levels, collapse = ", "))
}

zonal_sf <- zonal_sf %>%
  mutate(anomaly = ifelse(is.na(allowed_class), NA, observed_class != allowed_class))

# ------------------------------------------------------------
# 7 GEOMETRY CENTROIDS + REVERSE GEOCODING
# ------------------------------------------------------------
zonal_sf <- zonal_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

anomalies_sf <- zonal_sf %>% filter(anomaly == TRUE)

if (nrow(anomalies_sf) == 0) {
  message("No real anomalies – skipping geocoding.")
  anomalies_geocoded <- anomalies_sf %>% mutate(address = "No anomalies")
} else {
  message("Reverse-geocoding ", nrow(anomalies_sf), " anomalies...")
  if (nrow(anomalies_sf) > 50) message("Heads-up: many points; geocoding may take a while.")
  
  # anomalies_geocoded <- tryCatch({
  #   tidygeocoder::reverse_geocode(anomalies_sf,
  #                                 lat = lat, long = lon,
  #                                 method = "osm",
  #                                 address = address,
  #                                 full_results = FALSE)
  # }, error = function(e) {
  #   message("Geocoding failed: ", e$message)
  #   anomalies_sf %>% mutate(address = NA_character_)
  # })
  
  
  anomalies_geocoded <- tryCatch({
    df <- tidygeocoder::reverse_geocode(
      anomalies_sf,
      lat = lat, long = lon,
      method = "osm",
      address = address,
      full_results = FALSE
    )
    
    # Re-attach geometry
    if (!inherits(df, "sf")) {
      df <- df %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
    }
    df
  }, error = function(e) {
    message("⚠️  Geocoding failed: ", e$message)
    anomalies_sf %>% mutate(address = NA_character_)
  })
  
  
  if (!inherits(anomalies_geocoded, "sf")) {
    anomalies_geocoded <- sf::st_as_sf(anomalies_geocoded,
                                       coords = c("lon", "lat"),
                                       crs = 4326)
  }
  
  
  
  
  
  
  if (!"address" %in% names(anomalies_geocoded))
    anomalies_geocoded$address <- NA_character_
}

# Merge address back into full zonal_sf
zonal_sf <- zonal_sf %>%
  left_join(
    anomalies_geocoded %>% st_drop_geometry() %>% select(lon, lat, address),
    by = c("lon", "lat")
  )








# ------------------------------------------------------------
# 8 PLANNER-FRIENDLY TABLE
# ------------------------------------------------------------
planner_table <- anomalies_geocoded %>%
  st_drop_geometry() %>%
  select(
    Zone = zone_type,
    `Observed Class` = observed_class,
    `Allowed Class` = allowed_class,
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
# 9 INTERACTIVE MAP (Leaflet via mapview) — FIXED POPUP
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
    cex = 7,
    layer.name = "Anomalies (2024)",
    popup = popupTableSafe(
      anomalies_geocoded,
      zcol = c("zone_type", "observed_class", "allowed_class", "address", "lon", "lat")
    )
  )
  combined_map <- base_map + anom_map
} else {
  combined_map <- base_map
  combined_map@map <- combined_map@map %>%
    leaflet::addControl(html = "<h4 style='color:green;'>No anomalies detected</h4>",
                        position = "topright")
}

# ------------------------------------------------------------
# 10 DEMO MODE (Optional) — FIXED POPUP
# ------------------------------------------------------------
if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    data.frame(
      zone_type = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
      observed_class = c("Crops", "Built Area", "Bare Ground", "Crops", "Trees"),
      allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area"),
      lon = c(36.8219, 36.7892, 37.0010, 36.9055, 36.8452),
      lat = c(-1.2921, -1.3051, -0.9965, -1.2341, -1.3102),
      address = c("Nairobi CBD", "Kilimani", "Thika Road", "Athi River", "Donholm")
    ),
    coords = c("lon", "lat"), crs = 4326
  ) %>% mutate(anomaly = TRUE)
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    cex = 9,
    layer.name = "DEMO Fake Anomalies",
    popup = popupTableSafe(
      demo_pts,
      zcol = c("zone_type", "observed_class", "allowed_class", "address")
    )
  )
  
  combined_map <- combined_map + demo_map
}

# ------------------------------------------------------------
# 11 OUTPUTS
# ------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

mapview::mapshot(combined_map,
                 file = "output/anomalies_interactive_map.html",
                 selfcontained = TRUE)

dt_widget <- datatable(
  planner_table,
  escape = FALSE,
  extensions = c('Buttons', 'Scroller'),
  options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  ),
  rownames = FALSE,
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size: 1.2em;',
    "Land-Use Anomalies – 2024 (Planner Report)"
  )
)

saveWidget(dt_widget,
           file = "output/anomalies_planner_table.html",
           selfcontained = TRUE)

st_write(zonal_sf, "output/land_use_anomalies_full.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded,
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
} else {
  empty_sf <- st_sf(address = character(), geometry = st_sfc(crs = 4326))
  st_write(empty_sf,
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
}

# # ------------------------------------------------------------
# # 12 SUMMARY
# # ------------------------------------------------------------
# message("\n=== PLANNER REPORT READY ===")
# message("Interactive Map : output/anomalies_interactive_map.html")
# message("Planner Table (HTML) : output/anomalies_planner_table.html")
# message("Full GeoPackage : output/land_use_anomalies_full.gpkg")
# message("Anomalies GeoPackage : output/land_use_anomalies_geocoded.gpkg")
# if (isTRUE(demo_mode)) message("Demo Layer : included (orange points)")
# message("Total REAL anomalies : ", nrow(anomalies_geocoded))


# ------------------------------------------------------------
# 12 SUMMARY + AUTO-OPEN BROWSER
# ------------------------------------------------------------
message("\n=== PLANNER REPORT READY ===")
message("Interactive Map : output/anomalies_interactive_map.html")
message("Planner Table (HTML) : output/anomalies_planner_table.html")
if (isTRUE(demo_mode)) message("Demo Layer : included (orange points)")
message("Total REAL anomalies : ", nrow(anomalies_geocoded))

# Open the map immediately after the script finishes
browseURL(file.path(getwd(), "output/anomalies_interactive_map.html"))

# (Optional) open the table in a second tab
# browseURL(file.path(getwd(), "output/anomalies_planner_table.html"))
