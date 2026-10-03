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

# ---- T lines (subway + light rail: Red, Orange, Blue, Green, Mattapan) ----
# route_type 0 = light rail (Green Line branches, Mattapan), 1 = heavy rail.
# The Silver Line is a bus (route_type 3) in the feed, so it isn't included.
subway_routes <- gtfs$routes %>%
  filter(route_type %in% c(0, 1)) %>%
  transmute(
    route_id,
    route_name = route_long_name,
    route_color = paste0("#", route_color)
  )

subway_trips <- gtfs$trips %>%
  filter(route_id %in% subway_routes$route_id) %>%
  distinct(trip_id, route_id, shape_id)

# One line per shape, tagged with its route's name and official color
subway_shapes_sf <- gtfs$shapes %>%
  filter(shape_id %in% subway_trips$shape_id) %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id) %>%
  summarize(
    geometry = st_sfc(st_linestring(cbind(shape_pt_lon, shape_pt_lat))),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326) %>%
  left_join(distinct(subway_trips, shape_id, route_id), by = "shape_id") %>%
  distinct(shape_id, .keep_all = TRUE) %>%
  left_join(subway_routes, by = "route_id")

# Stations: platforms served by T trips, resolved to their parent station
subway_platforms <- gtfs$stop_times %>%
  filter(trip_id %in% subway_trips$trip_id) %>%
  distinct(trip_id, stop_id) %>%
  left_join(distinct(subway_trips, trip_id, route_id), by = "trip_id") %>%
  distinct(stop_id, route_id) %>%
  left_join(select(gtfs$stops, stop_id, parent_station), by = "stop_id") %>%
  mutate(station_id = if_else(!is.na(parent_station) & parent_station != "", parent_station, stop_id))

# Each station lists the lines serving it; a one-line station takes that
# line's color, a transfer station (e.g. Park Street) is drawn neutral
subway_station_lines <- subway_platforms %>%
  left_join(subway_routes, by = "route_id") %>%
  mutate(line = sub(" Line.*$", "", route_name)) %>% # "Green Line B" -> "Green"
  distinct(station_id, line, route_color) %>%
  group_by(station_id) %>%
  summarize(
    lines = paste(sort(unique(line)), collapse = ", "),
    station_color = if (n_distinct(route_color) == 1) first(route_color) else "#5F6B7A",
    .groups = "drop"
  )

subway_stations_sf <- gtfs$stops %>%
  filter(stop_id %in% subway_station_lines$station_id) %>%
  select(stop_id, stop_name, municipality, stop_lat, stop_lon) %>%
  left_join(subway_station_lines, by = c("stop_id" = "station_id")) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)

saveRDS(subway_shapes_sf, file = "data/subway_shapes_sf.rds")
saveRDS(subway_stations_sf, file = "data/subway_stations_sf.rds")
