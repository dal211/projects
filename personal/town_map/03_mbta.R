library(tidytransit)
library(dplyr)
library(sf)

# 1. Read the MBTA GTFS feed
gtfs_url <- "https://cdn.mbta.com/MBTA_GTFS.zip"
gtfs    <- read_gtfs(gtfs_url)
# str(gtfs$shapes)

# which routes are commuter rail?
commuter_routes <- gtfs$routes %>%
  filter(route_type == 2) %>%
  pull(route_id)

# now get the shape_ids used by those routes via the trips table
commuter_shape_ids <- gtfs$trips %>%
  filter(route_id %in% commuter_routes) %>%
  pull(shape_id) %>%
  unique()

shapes_sf <- gtfs$shapes %>%
  # keep only the shapes we need
  filter(shape_id %in% commuter_shape_ids) %>%
  # order by sequence so lines draw in the right order
  arrange(shape_id, shape_pt_sequence) %>%
  # one LINESTRING per shape_id
  group_by(shape_id) %>%
  summarize(
    geometry = st_sfc(
      st_linestring(
        cbind(shape_pt_lon, shape_pt_lat)
      )
    ),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326)

# saveRDS(shapes_sf, file = "data/shapes_sf.rds")

# ---- Commuter rail stations ----
# trips on commuter rail routes -> platform-level stop_ids actually served
commuter_trip_ids <- gtfs$trips %>%
  filter(route_id %in% commuter_routes) %>%
  pull(trip_id) %>%
  unique()

commuter_platform_ids <- gtfs$stop_times %>%
  filter(trip_id %in% commuter_trip_ids) %>%
  pull(stop_id) %>%
  unique()

# resolve each platform up to its parent station (location_type == 1) so each
# station shows once, not once per platform/track
platform_stops <- gtfs$stops %>% filter(stop_id %in% commuter_platform_ids)
station_ids <- unique(ifelse(
  !is.na(platform_stops$parent_station) & platform_stops$parent_station != "",
  platform_stops$parent_station,
  platform_stops$stop_id
))

commuter_stations_sf <- gtfs$stops %>%
  filter(stop_id %in% station_ids) %>%
  select(stop_id, stop_name, municipality, stop_lat, stop_lon) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)

# saveRDS(commuter_stations_sf, file = "data/commuter_stations_sf.rds")
