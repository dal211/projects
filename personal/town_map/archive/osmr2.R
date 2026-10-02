library(sf)
library(osrm)

# 1. load and reproject everything to WGS84
towns_sf <- readRDS("data/towns_sf.rds") %>% 
  st_transform(4326)

croton_sf <- st_sf(
  id = "Croton-on-Hudson",
  geometry = st_sfc(st_point(c(-73.891743, 41.218139)), crs = 4326)
)

# 2. swap in your 'centroid' column as the active geometry
towns_centroids <- st_set_geometry(towns_sf, "centroid")

# 3a. get driving DISTANCES (meters)
tbl_dist <- osrmTable(
  src     = towns_centroids,
  dst     = croton_sf,
  measure = "distance"
)

# 3b. get driving DURATIONS (seconds) – this is also the default
tbl_dur <- osrmTable(
  src     = towns_centroids,
  dst     = croton_sf,
  measure = "duration"
)

# 4. pull out the one‐column vectors
towns_sf$dist_m <- tbl_dist$distances[, 1]
towns_sf$dist_mi <- towns_sf$dist_m / 1609.34  # meters to miles


