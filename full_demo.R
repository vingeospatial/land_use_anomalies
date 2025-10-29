# ============================================================
# 🔧 1) Libraries
# ============================================================
library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)
library(geojsonio)
library(tibble)
library(tmap)         # interactive map
library(tidygeocoder) # reverse geocoding

# Toggle: show a fake-anomalies demo layer as well?
demo_mode <- TRUE

# ============================================================
# 🧠 2) Set Python Environment for rgee
# ============================================================
reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python", required = TRUE)
reticulate::py_config()

# ============================================================
# 🧹 3) Function to ensure authentication is valid
# ============================================================
ensure_ee_auth <- function(email, project) {
  tryCatch({
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project, quiet = TRUE)
    message("✅ Earth Engine initialized successfully for: ", email)
  }, error = function(e) {
    message("⚠️ EE credentials appear invalid or expired. Re-authenticating...")
    
    ee_clean_user_credentials()
    googledrive::drive_deauth()
    unlink("~/.config/earthengine", recursive = TRUE, force = TRUE)
    unlink("~/.config/rgee", recursive = TRUE, force = TRUE)
    unlink("~/.R/gargle", recursive = TRUE, force = TRUE)
    
    ee_Authenticate(drive = TRUE, gcs = FALSE, auth_mode = "notebook")
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project)
    message("✅ Re-authenticated and initialized successfully.")
  })
}

# ============================================================
# 🚀 4) Initialize (auto re-authenticate if needed)
# ============================================================
ensure_ee_auth(
  email   = "sds6488292025@students.uonbi.ac.ke",
  project = "ee-sds6488292025"
)

# ============================================================
# 🧾 5) Confirm active EE project and credentials
# ============================================================
print(ee_user_info())        # shows project, email, asset home
print(ee_get_assethome())    # shows asset path, e.g. "projects/ee-.../assets"

# ============================================================
# 🌍 6) Load the planning zones GeoPackage
# ============================================================
zones <- st_read("data/national_plan_zones.gpkg") %>%
  st_make_valid() %>%
  st_transform(4326)

# ============================================================
# 🔁 7) Convert to EE FeatureCollection
# ============================================================
zones_ee <- sf_as_ee(zones)

# ============================================================
# 🛰️ 8) Pull DynamicWorld for 2024
# ============================================================
dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01", "2024-12-31")$
  filterBounds(zones_ee$geometry())$
  median()

dw_label <- dw$select("label")

# ============================================================
# 📊 9) Zonal mode (dominant DW class per polygon)
# ============================================================
zonal_stats <- dw_label$reduceRegions(
  collection = zones_ee,
  reducer    = ee$Reducer$mode(),
  scale      = 10,
  crs        = "EPSG:4326"
)

# ============================================================
# 📥 10) Bring results back to R
# ============================================================
zonal_sf <- ee_as_sf(zonal_stats, maxFeatures = 10000)

# ============================================================
# 🏷️ 11) Class names
# ============================================================
classes <- tibble(
  id    = 0:8,
  class = c("Water", "Trees", "Grass", "Flooded Vegetation",
            "Crops", "Shrub & Scrub", "Built Area",
            "Bare Ground", "Snow/Ice")
)

zonal_sf <- zonal_sf %>%
  left_join(classes, by = c("mode" = "id")) %>%
  rename(observed_class = class)

# ============================================================
# 🧭 12) Allowed mapping (planned vs observed)
# ============================================================
allowed_map <- tibble(
  zone_type     = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
  allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area")
)

# ============================================================
# 🚨 13) Detect anomalies
# ============================================================
zonal_sf <- zonal_sf %>%
  left_join(allowed_map, by = "zone_type") %>%
  mutate(
    anomaly = ifelse(is.na(allowed_class), NA, observed_class != allowed_class)
  )

# ============================================================
# 📈 14) Summary (initial)
# ============================================================
anomaly_summary <- zonal_sf %>%
  group_by(zone_type) %>%
  summarise(
    total_zones   = n(),
    anomalies     = sum(anomaly, na.rm = TRUE),
    anomaly_rate  = round(100 * anomalies / total_zones, 2),
    .groups = "drop"
  )
print(anomaly_summary)

# ============================================================
# 💾 15) Export (full results)
# ============================================================
dir.create("output", showWarnings = FALSE)
st_write(zonal_sf, "output/land_use_anomalies.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ============================================================
# 🗺️ 16) Quick EE map (original snippet preserved)
#  Note: filtering by 'anomaly' here won’t reflect R-side anomaly flag,
#  since that flag is added after ee_as_sf(). Kept to include your original.
# ============================================================
Map$centerObject(zones_ee)
Map$addLayer(dw_label,
             list(min = 0, max = 8, palette = c(
               "#419BDF","#397D49","#88B053","#7A87C6",
               "#E49635","#DFC35A","#C4281B","#A59B8F","#B39EB5")),
             "Dynamic World 2024")
Map$addLayer(zones_ee$filter(ee$Filter$eq("anomaly", TRUE)),
             list(color = "red", fillColor = "00000000"),
             "Anomaly Zones (EE placeholder)")

# ============================================================
# 🌍 17) Add anomaly visualization + location context (safe-final)
# ============================================================
# Compute centroid coordinates
zonal_sf <- zonal_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# Extract anomalies (R-side)
anomalies_sf <- zonal_sf %>%
  filter(anomaly == TRUE) %>%
  select(zone_type, observed_class, allowed_class, lon, lat, geometry)

if (nrow(anomalies_sf) == 0) {
  message("✅ No anomalies detected — skipping geocoding.")
  anomalies_geocoded <- anomalies_sf %>% mutate(address = "No anomalies detected")
} else {
  message("📍 Reverse geocoding anomaly centroids (may take a minute)...")
  anomalies_geocoded <- tryCatch({
    result <- tidygeocoder::reverse_geocode(
      anomalies_sf,
      lat = lat, long = lon,
      method = "osm",
      address = address,
      full_results = FALSE
    )
    if (!"address" %in% names(result)) result$address <- NA_character_
    result
  }, error = function(e) {
    message("⚠️ Reverse geocoding failed: ", e$message)
    anomalies_sf$address <- NA_character_
    anomalies_sf
  })
}
if (!"address" %in% names(anomalies_geocoded)) {
  anomalies_geocoded$address <- NA_character_
}

# Merge address back
zonal_sf <- zonal_sf %>%
  left_join(
    anomalies_geocoded %>% st_drop_geometry() %>% dplyr::select(lon, lat, address),
    by = c("lon", "lat")
  )

# ============================================================
# 🧮 18) Anomaly summary table (final print)
# ============================================================
anomaly_summary <- zonal_sf %>%
  group_by(zone_type) %>%
  summarise(
    total_zones   = n(),
    anomalies     = sum(anomaly, na.rm = TRUE),
    anomaly_rate  = round(100 * anomalies / total_zones, 2),
    .groups = "drop"
  )
print(anomaly_summary)

# ============================================================
# 💾 19) Save enriched anomalies only
# ============================================================
if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded,
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
} else {
  # write an empty but valid layer for consistency
  st_write(zonal_sf %>% filter(FALSE) %>% mutate(address = character()),
           "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
}
message("✅ GeoPackage exported: output/land_use_anomalies_geocoded.gpkg")

# ============================================================
# 🧪 19b) DEMO: create fake anomalies (optional)
# ============================================================
if (isTRUE(demo_mode)) {
  test_sf <- st_as_sf(
    data.frame(
      id = 1:5,
      zone_type = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
      observed_class = c("Crops", "Built Area", "Bare Ground", "Crops", "Trees"),
      allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area"),
      anomaly = c(TRUE, TRUE, TRUE, FALSE, TRUE),
      lon = c(36.8219, 36.7892, 37.0010, 36.9055, 36.8452),
      lat = c(-1.2921, -1.3051, -0.9965, -1.2341, -1.3102),
      address = c("Nairobi", "Kilimani", "Thika", "Athi River", "Donholm")
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  st_write(test_sf, "output/test_land_use_anomalies.gpkg", delete_dsn = TRUE, quiet = TRUE)
  message("✅ Demo file written: output/test_land_use_anomalies.gpkg")
  
  # Simple dummy zone polygons near the markers (for visual context)
  zones_poly_demo <- st_as_sf(
    data.frame(
      zone_type = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
      lon = c(36.82, 36.78, 37.00, 36.90, 36.84),
      lat = c(-1.29, -1.30, -0.99, -1.23, -1.31)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  ) %>% st_buffer(dist = 0.03)
  
  assign("test_sf", test_sf, envir = .GlobalEnv)
  assign("zones_poly_demo", zones_poly_demo, envir = .GlobalEnv)
}

# ============================================================
# 🗺️ 20) Interactive anomaly map (tmap v4-safe)
# ============================================================
tmap_mode("view")

map_obj <- tm_shape(zones) +
  tm_polygons(fill = "zone_type", fill_alpha = 0.4, col = "grey") +
  tm_text("zone_type", size = 0.7)

# Real anomalies (if any)
if (nrow(zonal_sf %>% filter(anomaly == TRUE)) > 0) {
  map_obj <- map_obj +
    tm_shape(zonal_sf %>% filter(anomaly == TRUE)) +
    tm_symbols(
      col = "red", size = 0.2, border.col = "black",
      popup.vars = c(
        "Zone Type" = "zone_type",
        "Observed" = "observed_class",
        "Allowed" = "allowed_class",
        "Location" = "address",
        "Longitude" = "lon",
        "Latitude" = "lat"
      )
    )
} else {
  message("✅ No abnormal land use detected in the dataset.")
  map_obj <- map_obj + tm_layout(title = "✅ No abnormal land use detected for 2024")
}

# Demo layer (optional)
if (isTRUE(demo_mode)) {
  map_obj <- map_obj +
    tm_shape(zones_poly_demo) +
    tm_polygons(fill = "zone_type", fill_alpha = 0.15, col = "grey") +
    tm_shape(test_sf %>% filter(anomaly == TRUE)) +
    tm_symbols(
      col = "red", size = 0.25, border.col = "black",
      popup.vars = c(
        "Zone Type" = "zone_type",
        "Observed Class" = "observed_class",
        "Allowed Class" = "allowed_class",
        "Location" = "address"
      ),
      legend.show = TRUE
    ) +
    tm_layout(title = "🚨 Demo: Detected Land Use Anomalies (Fake Data)")
}

# Show the interactive map
map_obj

# ============================================================
# 🧾 21) Console summary report
# ============================================================
total_anomalies <- nrow(anomalies_geocoded)
message("🔍 Total REAL anomalies detected: ", total_anomalies)
message("🗺️ Real anomalies GeoPackage: output/land_use_anomalies_geocoded.gpkg")
if (isTRUE(demo_mode)) {
  message("🧪 Demo anomalies GeoPackage: output/test_land_use_anomalies.gpkg")
}
