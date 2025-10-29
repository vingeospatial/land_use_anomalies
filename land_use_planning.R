library(rgee)
library(sf)
library(dplyr)
library(googledrive)
library(reticulate)

reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python", required = TRUE)
reticulate::py_config()

ee_clean_user_credentials()
googledrive::drive_deauth()

unlink("~/.config/earthengine", recursive = TRUE, force = TRUE)
unlink("~/.config/rgee", recursive = TRUE, force = TRUE)
unlink("~/.R/gargle", recursive = TRUE, force = TRUE)

ee_Authenticate(drive = TRUE, gcs = FALSE, auth_mode = "notebook")

ee_Initialize(
  email = "sds6488292025@students.uonbi.ac.ke",
  drive = TRUE,
  gcs = FALSE,
  project = "ee-sds6488292025"
)


# 4. TEST
print(ee_get_project())
## -------------------------------------------------
## 4. (Optional) Force reticulate to use the conda env
## -------------------------------------------------
# If you started R **outside** the activated conda env, run:
# reticulate::use_python("/home/omuka/miniconda3/envs/rgee_env/bin/python",
#                        required = TRUE)

## -------------------------------------------------
## 5. Load the planning zones (GeoPackage)
## -------------------------------------------------
zones <- st_read("data/national_plan_zones.gpkg") %>%
  st_make_valid() %>%
  st_transform(4326)   # EE works in EPSG:4326

## -------------------------------------------------
## 6. Convert to EE FeatureCollection
## -------------------------------------------------
zones_ee <- sf_as_ee(zones)

## -------------------------------------------------
## 7. Pull DynamicWorld for 2024, Nairobi area
## -------------------------------------------------
dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterDate("2024-01-01", "2024-12-31")$
  filterBounds(zones_ee$geometry())$   # use the whole AOI
  median()                             # yearly composite

dw_label <- dw$select("label")        # 0-8 class IDs

## -------------------------------------------------
## 8. Zonal mode (most frequent class per polygon)
## -------------------------------------------------
zonal_stats <- dw_label$reduceRegions(
  collection = zones_ee,
  reducer    = ee$Reducer$mode(),
  scale      = 10,      # Sentinel-2 native resolution
  crs        = "EPSG:4326"
)

## -------------------------------------------------
## 9. Bring results back to R as sf
## -------------------------------------------------
zonal_sf <- ee_as_sf(zonal_stats, maxFeatures = 10000)

## -------------------------------------------------
## 10. Class name look-up table
## -------------------------------------------------
classes <- tibble::tibble(
  id    = 0:8,
  class = c("Water", "Trees", "Grass", "Flooded Vegetation",
            "Crops", "Shrub & Scrub", "Built Area",
            "Bare Ground", "Snow/Ice")
)

## -------------------------------------------------
## 11. Join observed class name
## -------------------------------------------------
zonal_sf <- zonal_sf %>%
  rename(mode = `.mode`) %>%               # column created by mode()
  left_join(classes, by = c("mode" = "id")) %>%
  rename(observed_class = class)

## -------------------------------------------------
## 12. Define allowed mapping (planned → allowed DW class)
## -------------------------------------------------
allowed_map <- tibble::tibble(
  zone_type     = c("URBAN", "AGRICULTURAL", "FOREST", "WETLAND", "INDUSTRIAL"),
  allowed_class = c("Built Area", "Crops", "Trees", "Flooded Vegetation", "Built Area")
)

## -------------------------------------------------
## 13. Detect anomalies
## -------------------------------------------------
zonal_sf <- zonal_sf %>%
  left_join(allowed_map, by = "zone_type") %>%
  mutate(
    anomaly = ifelse(is.na(allowed_class), NA,
                     observed_class != allowed_class)
  )

## -------------------------------------------------
## 14. Summary table
## -------------------------------------------------
anomaly_summary <- zonal_sf %>%
  group_by(zone_type) %>%
  summarise(
    total_zones   = n(),
    anomalies     = sum(anomaly, na.rm = TRUE),
    anomaly_rate  = round(100 * anomalies / total_zones, 2),
    .groups = "drop"
  )

print(anomaly_summary)

## -------------------------------------------------
## 15. Export results
## -------------------------------------------------
dir.create("output", showWarnings = FALSE)
st_write(zonal_sf, "output/land_use_anomalies.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

## -------------------------------------------------
## 16. Quick interactive map (optional)
## -------------------------------------------------
Map$centerObject(zones_ee)
Map$addLayer(dw_label,
             list(min = 0, max = 8, palette = c(
               "#419BDF","#397D49","#88B053","#7A87C6",
               "#E49635","#DFC35A","#C4281B","#A59B8F","#B39EB5")),
             "Dynamic World 2024")
Map$addLayer(zones_ee$filter(ee$Filter$eq("anomaly", TRUE)),
             list(color = "red", fillColor = "00000000"),
             "Anomaly Zones")



