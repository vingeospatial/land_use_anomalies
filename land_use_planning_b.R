# ============================================================
# 🔧 1. Libraries
# ============================================================
library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)
library(geojsonio)

# ============================================================
# 🧠 2. Set Python Environment for rgee
# ============================================================
reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python", required = TRUE)
reticulate::py_config()

# ============================================================
# 🧹 3. Function to ensure authentication is valid
# ============================================================
ensure_ee_auth <- function(email, project) {
  tryCatch({
    # Try initializing EE
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project, quiet = TRUE)
    message("✅ Earth Engine initialized successfully for: ", email)
  }, error = function(e) {
    message("⚠️ EE credentials appear invalid or expired. Re-authenticating...")
    
    # Clean up any bad credentials
    ee_clean_user_credentials()
    googledrive::drive_deauth()
    unlink("~/.config/earthengine", recursive = TRUE, force = TRUE)
    unlink("~/.config/rgee", recursive = TRUE, force = TRUE)
    unlink("~/.R/gargle", recursive = TRUE, force = TRUE)
    
    # Authenticate again
    ee_Authenticate(drive = TRUE, gcs = FALSE, auth_mode = "notebook")
    
    # Re-initialize
    ee_Initialize(email = email, drive = TRUE, gcs = FALSE, project = project)
    message("✅ Re-authenticated and initialized successfully.")
  })
}

# ============================================================
# 🚀 4. Initialize (auto re-authenticate if needed)
# ============================================================
ensure_ee_auth(
  email   = "sds6488292025@students.uonbi.ac.ke",
  project = "ee-sds6488292025"
)

# ============================================================
# 🧾 5. Confirm active EE project and credentials
# ============================================================
print(ee_user_info())        # shows project, email, asset home
print(ee_get_assethome())    # shows asset path, e.g. "projects/ee-sds6488292025/assets"

# ============================================================
# 🌍 6. Load the planning zones GeoPackage
# ============================================================
zones <- st_read("data/national_plan_zones.gpkg") %>%
  st_make_valid() %>%
  st_transform(4326)

# ============================================================
# 🔁 7. Convert to EE FeatureCollection
# ============================================================
zones_ee <- sf_as_ee(zones)

# ============================================================
# 🛰️ 8. Pull DynamicWorld for 2024
# ============================================================
dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01", "2024-12-31")$
  filterBounds(zones_ee$geometry())$
  median()

dw_label <- dw$select("label")

# ============================================================
# 📊 9. Zonal mode (dominant class per polygon)
# ============================================================
zonal_stats <- dw_label$reduceRegions(
  collection = zones_ee,
  reducer    = ee$Reducer$mode(),
  scale      = 10,
  crs        = "EPSG:4326"
)

# ============================================================
# 📥 10. Bring results back to R
# ============================================================
zonal_sf <- ee_as_sf(zonal_stats, maxFeatures = 10000)

# ============================================================
# 🏷️ 11. Class names
# ============================================================
classes <- tibble::tibble(
  id    = 0:8,
  class = c("Water", "Trees", "Grass", "Flooded Vegetation",
            "Crops", "Shrub & Scrub", "Built Area",
            "Bare Ground", "Snow/Ice")
)

zonal_sf <- zonal_sf %>%
  # rename(mode = `.mode`) %>%
  left_join(classes, by = c("mode" = "id")) %>%
  rename(observed_class = class)

# ============================================================
# 🧭 12. Allowed mapping (planned vs observed)
# ============================================================
allowed_map <- tibble::tibble(
  zone_type     = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
  allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area")
)

# ============================================================
# 🚨 13. Detect anomalies
# ============================================================
zonal_sf <- zonal_sf %>%
  left_join(allowed_map, by = "zone_type") %>%
  mutate(
    anomaly = ifelse(is.na(allowed_class), NA, observed_class != allowed_class)
  )

# ============================================================
# 📈 14. Summary
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
# 💾 15. Export
# ============================================================
dir.create("output", showWarnings = FALSE)
st_write(zonal_sf, "output/land_use_anomalies.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ============================================================
# 🗺️ 16. Quick map
# ============================================================
Map$centerObject(zones_ee)
Map$addLayer(dw_label,
             list(min = 0, max = 8, palette = c(
               "#419BDF","#397D49","#88B053","#7A87C6",
               "#E49635","#DFC35A","#C4281B","#A59B8F","#B39EB5")),
             "Dynamic World 2024")
Map$addLayer(zones_ee$filter(ee$Filter$eq("anomaly", TRUE)),
             list(color = "red", fillColor = "00000000"),
             "Anomaly Zones")





# ============================================================
# 🌍 17. Add anomaly visualization and location context (safe-final)
# ============================================================
library(tmap)
library(tidygeocoder)

# Compute centroid coordinates
zonal_sf <- zonal_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# Extract anomalies
anomalies_sf <- zonal_sf %>%
  filter(anomaly == TRUE) %>%
  select(zone_type, observed_class, allowed_class, lon, lat)

if (nrow(anomalies_sf) == 0) {
  message("✅ No anomalies detected — skipping geocoding.")
  anomalies_geocoded <- anomalies_sf %>%
    mutate(address = "No anomalies detected")
} else {
  message("📍 Reverse geocoding anomaly centroids (may take a minute)...")
  
  anomalies_geocoded <- tryCatch({
    result <- tidygeocoder::reverse_geocode(
      anomalies_sf,
      lat = lat,
      long = lon,
      method = "osm",
      address = address,
      full_results = FALSE
    )
    
    # Ensure address column exists
    if (!"address" %in% names(result)) {
      result$address <- NA_character_
    }
    
    result
  }, error = function(e) {
    message("⚠️ Reverse geocoding failed: ", e$message)
    anomalies_sf$address <- NA_character_
    anomalies_sf
  })
}

# Make sure the address column exists (even if all NA)
if (!"address" %in% names(anomalies_geocoded)) {
  anomalies_geocoded$address <- NA_character_
}

# Merge geocoded results back safely
zonal_sf <- zonal_sf %>%
  left_join(
    anomalies_geocoded %>%
      st_drop_geometry() %>%
      dplyr::select(lon, lat, address),
    by = c("lon", "lat")
  )



# ============================================================
# 🧮 18. Anomaly summary table
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
# 💾 19. Save enriched data for testing
# ============================================================
dir.create("output", showWarnings = FALSE)

# Export only anomalies with geocode and center coordinates
st_write(
  anomalies_geocoded,
  "output/land_use_anomalies_geocoded.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

message("✅ GeoPackage exported to: output/land_use_anomalies_geocoded.gpkg")

# # ============================================================
# # 🗺️ 20. Interactive anomaly map (local view)
# # ============================================================
# tmap_mode("view")
# 
# tm_shape(zones) +
#   tm_polygons("zone_type", alpha = 0.3, border.col = "grey") +
#   tm_shape(zonal_sf %>% filter(anomaly == TRUE)) +
#   tm_symbols(
#     col = "red",
#     size = 0.15,
#     border.col = "black",
#     popup.vars = c(
#       "Zone Type" = "zone_type",
#       "Observed" = "observed_class",
#       "Allowed" = "allowed_class",
#       "Location" = "address",
#       "Longitude" = "lon",
#       "Latitude" = "lat"
#     )
#   ) +
#   tm_layout(
#     title = "🚨 Detected Land Use Anomalies (Dynamic World vs. Zoning)",
#     legend.outside = TRUE
#   )


# ============================================================
# 🗺️ 20. Interactive anomaly map (v4-safe)
# ============================================================
library(tmap)
tmap_mode("view")

if (nrow(zonal_sf %>% filter(anomaly == TRUE)) == 0) {
  message("✅ No abnormal land use detected in the dataset.")
  
  tm_shape(zones) +
    tm_polygons(fill = "zone_type", fill_alpha = 0.4, col = "grey") +
    tm_text("zone_type", size = 0.7) +
    tm_layout(title = "✅ No abnormal land use detected for 2024") +
    tmap_options(check.and.fix = TRUE)
  
} else {
  tm_shape(zones) +
    tm_polygons(fill = "zone_type", fill_alpha = 0.3, col = "grey") +
    tm_shape(zonal_sf %>% filter(anomaly == TRUE)) +
    tm_symbols(
      col = "red",
      size = 0.15,
      border.col = "black",
      popup.vars = c(
        "Zone Type" = "zone_type",
        "Observed" = "observed_class",
        "Allowed" = "allowed_class",
        "Location" = "address",
        "Longitude" = "lon",
        "Latitude" = "lat"
      )
    ) +
    tm_layout(title = "🚨 Detected Land Use Anomalies (Dynamic World vs. Zoning)")
}






# ============================================================
# 🧾 21. Console summary report
# ============================================================
total_anomalies <- nrow(anomalies_geocoded)
message("🔍 Total anomalies detected: ", total_anomalies)
message("🗺️ GeoPackage file created: output/land_use_anomalies_geocoded.gpkg")










