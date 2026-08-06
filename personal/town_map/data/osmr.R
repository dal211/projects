# install.packages(c("readxl","sf","osrm","dplyr"))
library(readxl)
library(sf)
library(osrm)
library(dplyr)
library(tidyverse)

towns_sf <- readRDS("data/towns_sf.rds")
towns_sf <- st_simplify(towns_sf, dTolerance = 100) 

# 3) Compute centroids of each MULTIPOLYGON
towns_ct <- towns_sf %>%
  st_centroid(of_largest_polygon = TRUE)

# 4) Prepare a one‐row sf for Croton‐on‐Hudson
croton_sf <- st_sf(
  id       = "Croton-on-Hudson",
  geometry = st_sfc(st_point(c(-73.891743, 41.218139)), crs = 4326)
)

# 5) Query OSRM for driving-distance matrix
#    (distances in kilometers, durations in minutes)
tbl <- osrmTable(src = towns_ct, dst = croton_sf)

# 6) Extract & assemble into a tibble
# dist_km    <- tbl$distances[,1]
# dur_min    <- tbl$durations[,1]
# out <- tibble(
#   town           = towns_ct$town_name,
#   distance_km    = dist_km,
#   distance_miles = dist_km * 0.621371,
#   duration_min   = dur_min
# )

library(purrr)

# Split towns into batches of 25 (or another safe number)
town_batches <- split(towns_ct, ceiling(seq_len(nrow(towns_ct)) / 25))

# Loop through and call osrmTable on each batch
results <- map_dfr(town_batches, function(batch) {
  Sys.sleep(1)  # be polite to the server
  tbl <- try(osrmTable(src = batch, dst = croton_sf), silent = TRUE)
  
  if (inherits(tbl, "try-error") || nrow(tbl$distances) == 0) {
    return(tibble(
      town = batch$town_name,
      distance_km = NA_real_,
      distance_miles = NA_real_,
      duration_min = NA_real_
    ))
  }
  
  tibble(
    town = batch$town_name,
    distance_km = tbl$distances[,1],
    distance_miles = tbl$distances[,1] * 0.621371,
    duration_min = tbl$durations[,1]
  )
})


# 7) Save for later
saveRDS(out, "towns_to_croton_osrm_distances.rds")


# Now `out` has one row per town with driving distance & time
