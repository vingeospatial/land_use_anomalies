# ============================================================
# LAND USE ANOMALY PIPELINE — PRODUCTION READY + DASHBOARD (FINAL FIXED)
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
zones_path <- "data/national_plan_zone_c.gpkg"
if (!file.exists(zones_path)) stop("Input not found: ", zones_path)

zones <- st_read(zones_path, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

if (!("zone_type" %in% names(zones))) {
  stop("The zones layer must contain a 'zone_type' column.")
}

zones_ee <- sf_as_ee(zones)

# ------------------------------------------------------------
# 4) GET DYNAMIC WORLD DATA (Temporal Mode)
# ------------------------------------------------------------
# Use multiple Dynamic World images to compute modal class
dw_mode <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(zones_ee$geometry())$
  filterDate("2024-01-01", Sys.Date() %>% as.character())$
  select("label")$
  reduce(ee$Reducer$mode())

# ESA WorldCover fallback (optional)
# esa <- ee$Image("ESA/WorldCover/v200/2023")$select("Map")
# combined <- dw_mode$unmask(esa)

# Zonal statistics
zonal_stats <- dw_mode$reduceRegions(
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
  cands <- c("mode", ".mode", "label_mode", "label", "Map")
  m <- intersect(cands, nm)
  if (length(m) == 0) stop("Couldn't find a mode column. Found: ", paste(nm, collapse = ", "))
  m[1]
}
mode_col <- pick_mode_col(names(zonal_sf))
zonal_sf <- zonal_sf |> rename(mode = !!rlang::sym(mode_col))

# ------------------------------------------------------------
# 6) CLASS LABELS & ANOMALY LOGIC
# ------------------------------------------------------------
classes <- tibble(
  id = 0:8,
  class = c("Water","Trees","Grass","Flooded Vegetation",
            "Crops","Shrub & Scrub","Built Area","Bare Ground","Snow/Ice")
)

zonal_sf <- zonal_sf |>
  mutate(zone_type = toupper(trimws(zone_type)))

allowed_map <- tibble(
  zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
  allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area")
) |> mutate(zone_type = toupper(trimws(zone_type)))

zonal_sf <- zonal_sf |>
  mutate(zone_type_clean = case_when(
    grepl("URBAN", zone_type, ignore.case = TRUE) ~ "URBAN",
    grepl("AGRIC", zone_type, ignore.case = TRUE) ~ "AGRICULTURAL",
    grepl("FOREST", zone_type, ignore.case = TRUE) ~ "FOREST",
    grepl("WETLAND", zone_type, ignore.case = TRUE) ~ "WETLAND",
    grepl("INDUSTR", zone_type, ignore.case = TRUE) ~ "INDUSTRIAL",
    TRUE ~ "UNKNOWN"
  )) |>
  left_join(allowed_map, by = c("zone_type_clean" = "zone_type"))

zonal_sf <- zonal_sf |>
  left_join(classes, by = c("mode" = "id")) |>
  rename(observed_class = class)

missing_mask <- is.na(zonal_sf$allowed_class)
if (any(missing_mask, na.rm = TRUE)) {
  missing_types <- sort(unique(zonal_sf$zone_type[missing_mask]))
  message("Warning: ", sum(missing_mask, na.rm = TRUE),
          " features lack 'allowed_class'. Unmapped zone_type(s): ",
          paste(missing_types, collapse = ", "))
}

zonal_sf <- zonal_sf |>
  mutate(
    anomaly = case_when(
      is.na(observed_class) ~ FALSE,
      is.na(allowed_class) ~ FALSE,
      observed_class != allowed_class ~ TRUE,
      TRUE ~ FALSE
    )
  )

message("Observed classes: ")
print(table(zonal_sf$observed_class, useNA = "ifany"))

message("Allowed classes: ")
print(table(zonal_sf$allowed_class, useNA = "ifany"))

message("Anomaly summary: ")
print(table(zonal_sf$anomaly, useNA = "ifany"))

# ------------------------------------------------------------
# 7) GEOCODING ANOMALIES (FIXED)
# ------------------------------------------------------------
# if (!all(c("lon","lat") %in% names(zonal_sf))) {
#   zonal_sf <- zonal_sf |>
#     mutate(
#       centroid = st_centroid(geometry),
#       lon = st_coordinates(centroid)[, 1],
#       lat = st_coordinates(centroid)[, 2]
#     ) |>
#     select(-centroid)
# }
# 
# anomalies_sf <- zonal_sf |> filter(!is.na(anomaly) & anomaly == TRUE)
# 
# if (nrow(anomalies_sf) == 0) {
#   message("✅ No anomalies detected — skipping reverse geocoding.")
#   anomalies_geocoded <- anomalies_sf |> mutate(address = "No anomalies detected")
# } else {
#   message("📍 Reverse-geocoding ", nrow(anomalies_sf), " anomalies using OSM...")
#   coords_df <- anomalies_sf %>%
#     st_drop_geometry() %>%
#     select(zone_type, observed_class, allowed_class, lon, lat)
#   
#   geocoded_df <- tryCatch({
#     tidygeocoder::reverse_geocode(
#       .tbl = coords_df,
#       lat = lat,
#       long = lon,
#       method = "osm",
#       address = "address",
#       full_results = FALSE
#     )
#   }, error = function(e) {
#     message("Reverse geocoding failed: ", e$message)
#     coords_df %>% mutate(address = NA_character_)
#   })
#   
#   if ("long" %in% names(geocoded_df) && !("lon" %in% names(geocoded_df))) {
#     geocoded_df <- rename(geocoded_df, lon = long)
#   }
#   if (!"address" %in% names(geocoded_df)) geocoded_df$address <- NA_character_
#   
#   anomalies_geocoded <- st_as_sf(geocoded_df, coords = c("lon", "lat"), crs = 4326)
# }
# 
# zonal_sf <- zonal_sf %>%
#   left_join(anomalies_geocoded %>% st_drop_geometry() %>% select(lon, lat, address), by = c("lon", "lat"))


# ------------------------------------------------------------
# 7) GEOCODING ANOMALIES (FINAL FIX)
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
  message("✅ No anomalies detected — skipping reverse geocoding.")
  anomalies_geocoded <- anomalies_sf |> mutate(address = "No anomalies detected")
} else {
  message("📍 Reverse-geocoding ", nrow(anomalies_sf), " anomalies using OSM...")
  coords_df <- anomalies_sf %>%
    st_drop_geometry() %>%
    select(zone_type, observed_class, allowed_class, lon, lat)
  
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
  
  # ✅ Guarantee lon/lat columns exist after geometry conversion
  if (!all(c("lon","lat") %in% names(anomalies_geocoded))) {
    coords <- st_coordinates(anomalies_geocoded)
    anomalies_geocoded <- anomalies_geocoded %>%
      mutate(lon = coords[,1], lat = coords[,2])
  }
}

# ✅ Safe join back to zonal_sf
zonal_sf <- zonal_sf %>%
  left_join(anomalies_geocoded %>% st_drop_geometry() %>% select(lon, lat, address), by = c("lon", "lat"))





# ------------------------------------------------------------
# 8) PLANNER-FRIENDLY TABLE
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
# 9–14 (Dashboard, Save, Outputs)
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

if (isTRUE(demo_mode)) {
  demo_pts <- st_as_sf(
    data.frame(
      zone_type = c("URBAN","AGRICULTURAL","FOREST","WETLAND","INDUSTRIAL"),
      observed_class = c("Crops","Built Area","Bare Ground","Crops","Trees"),
      allowed_class = c("Built Area","Crops","Trees","Flooded Vegetation","Built Area"),
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
    popup = popupTableSafe(demo_pts, zcol = c("zone_type","observed_class","allowed_class","address"))
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

dir.create("output", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(combined_map@map, "output/anomalies_interactive_map.html", selfcontained = TRUE)

full_dt <- datatable(
  planner_table,
  escape = FALSE,
  extensions = c('Buttons','Scroller'),
  options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print'), deferRender = TRUE, scrollY = 400, scroller = TRUE),
  rownames = FALSE
)
htmlwidgets::saveWidget(full_dt, "output/anomalies_planner_table.html", selfcontained = TRUE)

preview_dt <- datatable(head(planner_table, 20), escape = FALSE, options = list(dom = 't', pageLength = 20, scrollY = "300px", paging = FALSE), rownames = FALSE)
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
st_write(zonal_sf, "output/land_use_anomalies_full.gpkg", delete_dsn = TRUE, quiet = TRUE)
if (nrow(anomalies_geocoded) > 0) {
  st_write(anomalies_geocoded, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)
} else {
  empty_sf <- st_sf(address = character(), geometry = st_sfc(crs = 4326))
  st_write(empty_sf, "output/land_use_anomalies_geocoded.gpkg", delete_dsn = TRUE, quiet = TRUE)
}

message("\n=== PLANNER DASHBOARD READY ===")
message("Open: output/planner_dashboard.html")
message("Total REAL anomalies: ", nrow(anomalies_geocoded))
if (isTRUE(demo_mode)) message("Demo mode: orange points are fake")
if (interactive()) try(browseURL(file.path(getwd(), "output/planner_dashboard.html")), silent = TRUE)
