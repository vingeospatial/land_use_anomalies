# ============================================================
# 🌍 LAND USE ANOMALY PIPELINE — PRODUCTION READY + SEARCH + CSV EXPORT
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
library(webshot)
library(leaflet.extras)   # 🔍 for search box

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
        "<table style='font-size:12px;'>",
        paste0("<tr><td><b>", names(row), ":</b></td>",
               "<td style='padding-left:6px;'>", unname(row), "</td></tr>", collapse = ""),
        "</table>"
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
    message("⚠️ Re-authenticating Google Earth Engine...")
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
  email   = "sds6488292025@students.uonbi.ac.ke",
  project = "ee-sds6488292025"
)

# ------------------------------------------------------------
# 3) LOAD PLANNING ZONES
# ------------------------------------------------------------
zones_path <- "data/national_plan_zone_c.gpkg"
if (!file.exists(zones_path)) stop("Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

if (!("zone_type" %in% names(zones))) {
  stop("The zones layer must contain a 'zone_type' column.")
}

zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4) GET DYNAMIC WORLD DATA (2024 median)
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
# 5) STANDARDIZE MODE COLUMN NAME
# ------------------------------------------------------------
pick_mode_col <- function(nm) {
  cands <- c("mode", ".mode", "label_mode", "label")
  m <- intersect(cands, nm)
  if (length(m) == 0) stop("Couldn't find a mode column in EE output. Found: ", paste(nm, collapse = ", "))
  m[1]
}
mode_col <- pick_mode_col(names(zonal_sf))
zonal_sf <- zonal_sf %>% rename(mode = !!rlang::sym(mode_col))

# ------------------------------------------------------------
# 6) CLASS LABELS & ANOMALY LOGIC
# ------------------------------------------------------------
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

zonal_sf <- zonal_sf %>% left_join(allowed_map, by = "zone_type")

missing_mask <- is.na(zonal_sf$allowed_class)
if (any(missing_mask, na.rm = TRUE)) {
  missing_types <- sort(unique(zonal_sf$zone_type[missing_mask]))
  message("⚠️  ", sum(missing_mask, na.rm = TRUE),
          " features lack 'allowed_class'. Unmapped zone_type values: ",
          paste(missing_types, collapse = ", "))
}

zonal_sf <- zonal_sf %>%
  mutate(anomaly = ifelse(is.na(allowed_class), NA, observed_class != allowed_class))

# ------------------------------------------------------------
# 7) CENTROIDS & REVERSE GEOCODING
# ------------------------------------------------------------
if (!all(c("lon","lat") %in% names(zonal_sf))) {
  zonal_sf <- zonal_sf %>%
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[,1],
      lat = st_coordinates(centroid)[,2]
    )
}

anomalies_sf <- zonal_sf %>% filter(isTRUE(anomaly))

if (nrow(anomalies_sf) == 0) {
  message("✅ No real anomalies – skipping geocoding.")
  anomalies_geocoded <- anomalies_sf %>% mutate(address = "No anomalies")
} else {
  message("📍 Reverse-geocoding ", nrow(anomalies_sf), " anomalies...")
  coords_df <- anomalies_sf %>%
    st_drop_geometry() %>%
    select(zone_type, observed_class, allowed_class, lon, lat)
  
  geocoded_df <- tryCatch({
    tidygeocoder::reverse_geocode(
      data        = coords_df,
      lat         = lat,
      long        = lon,
      method      = "osm",
      address     = address,
      full_results = FALSE
    )
  }, error = function(e) {
    message("⚠️  Geocoding failed: ", e$message)
    coords_df %>% mutate(address = NA_character_)
  })
  
  if ("long" %in% names(geocoded_df) && !("lon" %in% names(geocoded_df))) {
    geocoded_df <- geocoded_df %>% rename(lon = long)
  }
  if (!all(c("lon","lat") %in% names(geocoded_df))) {
    stop("Reverse geocoding output missing lon/lat columns.")
  }
  if (!("address" %in% names(geocoded_df))) {
    geocoded_df$address <- NA_character_
  }
  
  anomalies_geocoded <- sf::st_as_sf(geocoded_df, coords = c("lon","lat"), crs = 4326)
}

zonal_sf <- zonal_sf %>%
  left_join(
    anomalies_geocoded %>% st_drop_geometry() %>% select(lon, lat, address),
    by = c("lon","lat")
  )

# ------------------------------------------------------------
# 8) PLANNER-FRIENDLY TABLE
# ------------------------------------------------------------
planner_table <- anomalies_geocoded %>%
  st_drop_geometry() %>%
  select(
    Zone            = zone_type,
    `Observed Class`= observed_class,
    `Allowed Class` = allowed_class,
    Longitude       = lon,
    Latitude        = lat,
    Address         = address
  ) %>%
  mutate(
    `Google Maps Link` = paste0(
      "<a href='https://www.google.com/maps?q=", Latitude, ",", Longitude,
      "' target='_blank'>Open in Maps</a>"
    )
  )

# ------------------------------------------------------------
# 9) INTERACTIVE MAP
# ------------------------------------------------------------
base_map <- mapview(
  zones,
  zcol = "zone_type",
  alpha.regions = 0.25,
  layer.name = "Planning Zones",
  map.types = c("OpenStreetMap","Esri.WorldImagery")
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
      zcol = c("zone_type","observed_class","allowed_class","address","lon","lat")
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
# 10) DEMO MODE
# ------------------------------------------------------------
if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    data.frame(
      zone_type      = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      observed_class = c("Crops","Built Area","Bare Ground","Crops","Trees"),
      allowed_class  = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area"),
      lon            = c(36.8219,36.7892,37.0010,36.9055,36.8452),
      lat            = c(-1.2921,-1.3051,-0.9965,-1.2341,-1.3102),
      address        = c("Nairobi CBD","Kilimani","Thika Road","Athi River","Donholm")
    ),
    coords = c("lon","lat"), crs = 4326
  ) %>% mutate(anomaly = TRUE)
  
  demo_map <- mapview(
    demo_pts,
    col.regions = "orange",
    cex = 9,
    layer.name = "DEMO Fake Anomalies",
    popup = popupTableSafe(
      demo_pts,
      zcol = c("zone_type","observed_class","allowed_class","address")
    )
  )
  
  combined_map <- combined_map + demo_map
}

# ------------------------------------------------------------
# 11) OUTPUTS (HTML + CSV + GPKG)
# ------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# ✅ Add searchable map
combined_map@map <- combined_map@map %>%
  addSearchFeatures(
    targetGroups = c("Planning Zones", "Anomalies (2024)", "DEMO Fake Anomalies"),
    options = searchFeaturesOptions(
      zoom = 12,
      openPopup = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )

# ✅ Save the interactive Leaflet map directly as HTML
htmlwidgets::saveWidget(
  widget = combined_map@map,
  file = "output/anomalies_interactive_map.html",
  selfcontained = TRUE
)

# ✅ Planner table export (HTML)
dt_widget <- datatable(
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
  rownames = FALSE,
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size: 1.2em;',
    "Land-Use Anomalies – 2024 (Planner Report)"
  )
)

saveWidget(dt_widget, "output/anomalies_planner_table.html", selfcontained = TRUE)

# ✅ CSV export
planner_table_csv <- planner_table %>%
  mutate(`Google Maps Link` = paste0("https://www.google.com/maps?q=", Latitude, ",", Longitude))
write.csv(planner_table_csv, "output/anomalies_planner_table.csv", row.names = FALSE)

# ✅ GeoPackages
st_write(zonal_sf, "output/land_use_anomalies_full.gpkg", delete_dsn = TRUE, quiet = TRUE)
if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
} else {
  empty_sf <- st_sf(address = character(), geometry = st_sfc(crs = 4326))
  st_write(empty_sf, "output/land_use_anomalies_geocoded.gpkg",
           delete_dsn = TRUE, quiet = TRUE)
}

# ------------------------------------------------------------
# 12) SUMMARY + AUTO-OPEN
# ------------------------------------------------------------
message("\n✅ === PLANNER REPORT READY ===")
message("Interactive Map      : output/anomalies_interactive_map.html")
message("Planner Table (HTML) : output/anomalies_planner_table.html")
message("Planner Table (CSV)  : output/anomalies_planner_table.csv")
message("Full GeoPackage      : output/land_use_anomalies_full.gpkg")
message("Anomalies GeoPackage : output/land_use_anomalies_geocoded.gpkg")
if (isTRUE(demo_mode)) message("Demo Layer           : included (orange points)")
message("Total REAL anomalies : ", nrow(anomalies_geocoded))

try(browseURL(file.path(getwd(), "output/anomalies_interactive_map.html")), silent = TRUE)

