# Load packages
library(readxl)
library(sf)
library(osrm)
library(dplyr)
library(tidyverse)
library(purrr)

# Load town geometries and simplify
towns_sf <- readRDS("data/towns_sf.rds")
towns_sf <- st_simplify(towns_sf, dTolerance = 100)

# Compute centroids
towns_ct <- towns_sf %>%
  st_centroid(of_largest_polygon = TRUE)

# Croton-on-Hudson destination point
croton_sf <- st_sf(
  id = "Croton-on-Hudson",
  geometry = st_sfc(st_point(c(-73.891743, 41.218139)), crs = 4326)
)

# Split into batches of 25
town_batches <- split(towns_ct, ceiling(seq_len(nrow(towns_ct)) / 25))

# library(sf)
# library(dplyr)
# library(purrr)
# library(tibble)

# town_batches is a list of sf objects; each batch[[i]] has columns including town_name
# town_batch_df <- map2_dfr(
#   town_batches,                    # the list
#   seq_along(town_batches),         # batch index
#   ~ {
#     coords <- st_coordinates(.x)   # extract matrix of c(lon, lat)
#     tibble(
#       batch = .y,                  # batch number
#       town  = .x$town_name,        # your town_name column
#       lon   = coords[,1],
#       lat   = coords[,2]
#     )
#   }
# )
# 
# # Inspect the first few rows
# head(town_batch_df)


# Batch OSRM calls
results <- map_dfr(town_batches, function(batch) {
  Sys.sleep(1)
  tbl <- try(osrmTable(src = batch, dst = croton_sf), silent = TRUE)

  if (inherits(tbl, "try-error") ||
      is.null(tbl$distances) ||
      length(tbl$distances) == 0) {
    return(tibble(
      town           = batch$town_name,
      distance_km    = NA_real_,
      distance_miles = NA_real_,
      duration_min   = NA_real_
    ))
  }

  d_km <- as.vector(tbl$distances[,1])
  t_min <- as.vector(tbl$durations[,1])

  tibble(
    town           = batch$town_name,
    distance_km    = d_km,
    distance_miles = d_km * 0.621371,
    duration_min   = t_min
  )
})

# Save the final results
saveRDS(results, "towns_to_croton_osrm_distances.rds")
