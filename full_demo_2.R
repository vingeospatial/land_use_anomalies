# ============================================================
# 1)  LOAD LIBRARIES & SETTINGS
# ============================================================
library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)
library(geojsonio)
library(tibble)
library(tmap)
library(tidygeocoder)
library(mapview)      # <-- interactive Leaflet maps
library(DT)           # <-- nice HTML tables
library(htmlwidgets)  # <-- saveWidget

demo_mode <- TRUE

# ============================================================
# 2)  PYTHON & EE AUTH (unchanged from your script)
# ============================================================
reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python", required = TRUE)
ensure_ee_auth <- function(email, project) {
  tryCatch({
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project, quiet = TRUE)
    message("EE initialized")
  }, error = function(e) {
    message("Re-authenticating...")
    ee_clean_user_credentials()
    googledrive::drive_deauth()
    unlink("~/.config/earthengine", recursive = TRUE, force = TRUE)
    unlink("~/.config/rgee", recursive = TRUE, force = TRUE)
    unlink("~/.R/gargle", recursive = TRUE, force = TRUE)
    ee_Authenticate(drive = TRUE, gcs = FALSE, auth_mode = "notebook")
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project)
  })
}
ensure_ee_auth(email = "sds6488292025@students.uonbi.ac.ke",
               project = "ee-sds6488292025")

# ============================================================
# 3)  LOAD ZONES & RUN THE WHOLE PIPELINE (your code, trimmed)
# ============================================================
zones <- st_read("data/national_plan_zones.gpkg") %>%
  st_make_valid() %>% st_transform(4326)

zones_ee <- sf_as_ee(zones)

dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01","2024-12-31")$
  filterBounds(zones_ee$geometry())$median()$
  select("label")

zonal_stats <- dw$reduceRegions(
  collection = zones_ee,
  reducer = ee$Reducer$mode(),
  scale = 10,
  crs = "EPSG:4326"
)

zonal_sf <- ee_as_sf(zonal_stats, maxFeatures = 10000)

classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)
zonal_sf <- zonal_sf %>%
  left_join(classes, by = c("mode" = "id")) %>%
  rename(observed_class = class)

allowed_map <- tibble(
  zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
  allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area")
)

zonal_sf <- zonal_sf %>%
  left_join(allowed_map, by = "zone_type") %>%
  mutate(anomaly = ifelse(is.na(allowed_class), NA, observed_class != allowed_class))

# ---------------------------------------------------------
# 4)  CENTROIDS + REVERSE GEOCODING
# ---------------------------------------------------------
zonal_sf <- zonal_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  )

anomalies_sf <- zonal_sf %>% filter(anomaly == TRUE)

if (nrow(anomalies_sf) == 0) {
  message("No real anomalies – skipping geocoding")
  anomalies_geocoded <- anomalies_sf %>% mutate(address = "No anomalies")
} else {
  message("Reverse-geocoding ", nrow(anomalies_sf), " anomalies...")
  anomalies_geocoded <- tryCatch({
    tidygeocoder::reverse_geocode(anomalies_sf,
                                  lat = lat, long = lon,
                                  method = "osm",
                                  address = address,
                                  full_results = FALSE)
  }, error = function(e){
    message("Geocoding failed: ", e$message)
    anomalies_sf %>% mutate(address = NA_character_)
  })
  if (!"address" %in% names(anomalies_geocoded)) {
    anomalies_geocoded$address <- NA_character_
  }
}
# Merge address back to full table (optional)
zonal_sf <- zonal_sf %>%
  left_join(anomalies_geocoded %>% st_drop_geometry() %>% select(lon,lat,address),
            by = c("lon","lat"))

# ---------------------------------------------------------
# 5)  PREPARE PLANNER-FRIENDLY TABLE
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# 6)  INTERACTIVE MAP (Leaflet via mapview)
# ---------------------------------------------------------
# Base layer: planning zones (transparent)
base_map <- mapview(zones,
                    zcol = "zone_type",
                    alpha.regions = 0.25,
                    layer.name = "Planning Zones",
                    map.types = c("OpenStreetMap","Esri.WorldImagery"))

# Real anomalies (red circles)
if (nrow(anomalies_geocoded) > 0) {
  anom_map <- mapview(anomalies_geocoded,
                      zcol = NULL,
                      col.regions = "red",
                      cex = 7,
                      layer.name = "Anomalies (2024)",
                      popup = popupTable(
                        anomalies_geocoded,
                        zcol = c("zone_type","observed_class","allowed_class",
                                 "address","lon","lat"),
                        feature.id = FALSE,
                        row.numbers = FALSE
                      ))
  combined_map <- base_map + anom_map
} else {
  combined_map <- base_map
  combined_map@map <- combined_map@map %>%
    leaflet::addControl(html = "<h4 style='color:green;'>No anomalies detected</h4>",
                        position = "topright")
}

# OPTIONAL DEMO LAYER
if (isTRUE(demo_mode)) {
  # ---- fake points (same as your demo block) ----
  demo_pts <- st_as_sf(
    data.frame(
      zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      observed_class = c("Crops","Built Area","Bare Ground","Crops","Trees"),
      allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area"),
      lon = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102),
      address = c("Nairobi","Kilimani","Thika","Athi River","Donholm")
    ),
    coords = c("lon","lat"), crs = 4326
  ) %>% mutate(anomaly = TRUE)
  
  demo_map <- mapview(demo_pts,
                      col.regions = "orange",
                      cex = 9,
                      layer.name = "DEMO Fake Anomalies",
                      popup = popupTable(demo_pts,
                                         zcol = c("zone_type","observed_class",
                                                  "allowed_class","address"),
                                         feature.id = FALSE))
  
  combined_map <- combined_map + demo_map
}

# ---------------------------------------------------------
# 7)  SAVE MAP + TABLE
# ---------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# ---- HTML interactive map ----
mapview::mapshot(combined_map,
                 file = "output/anomalies_interactive_map.html",
                 selfcontained = TRUE)

# ---- HTML table (DT) ----
dt_widget <- datatable(
  planner_table,
  escape = FALSE,               # allow HTML links
  extensions = c('Buttons','Scroller'),
  options = list(
    dom = 'Bfrtip',
    buttons = c('copy','csv','excel','pdf','print'),
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

# ---- GeoPackages (already in your script) ----
st_write(zonal_sf,
         "output/land_use_anomalies_full.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded,
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
} else {
  # empty valid file
  st_write(st_sf(geometry = st_sfc(st_point()), address = character()),
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
}

# ---------------------------------------------------------
# 8)  CONSOLE SUMMARY
# ---------------------------------------------------------
message("\n=== PLANNER REPORT READY ===")
message("Interactive Map      : output/anomalies_interactive_map.html")
message("Planner Table (HTML) : output/anomalies_planner_table.html")
message("Full GeoPackage      : output/land_use_anomalies_full.gpkg")
message("Anomalies GeoPackage : output/land_use_anomalies_geocoded.gpkg")
if (isTRUE(demo_mode)) {
  message("Demo file            : output/test_land_use_anomalies.gpkg")
}
message("Total REAL anomalies : ", nrow(anomalies_geocoded))