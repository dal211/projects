library(tidytransit)
library(dplyr)

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

