library(shiny)
library(leaflet)
library(mapboxapi)
library(dplyr)
library(tigris)
library(readr)
library(sf)
library(rsconnect)
library(tidyverse)
library(styler)
library(tidycensus)
library(ggplot2)
library(usethis)
library(scales)
library(flexdashboard)
library(readxl)
library(openxlsx)
library(fuzzyjoin)
library(leaflet.extras)
library(googleway)
library(purrr)

googleway::set_key("GOOGLE_MAPS_API_KEY")

# ——————————————————————————————————————————————————
# 2) read your MULTIPOLYGON town shapes
# ——————————————————————————————————————————————————
towns_sf <- readRDS("data/towns_sf.rds")

# ——————————————————————————————————————————————————
# 3) compute centroids (ensures one POINT per town)
# ——————————————————————————————————————————————————
towns_centroids <- towns_sf %>%
  st_centroid(of_largest_polygon = TRUE)

# ——————————————————————————————————————————————————
# 4) extract lon/lat and town_name
# ——————————————————————————————————————————————————
towns_df <- towns_centroids %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  rename(name = town_name)  # use your actual column

# ——————————————————————————————————————————————————
# 5) prepare origins & destination
# ——————————————————————————————————————————————————
make_origin <- function(lat, lon) paste(lat, lon, sep = ",")
origins     <- make_origin(towns_df$lat, towns_df$lon)
destination <- "41.218139,-73.891743"  # Croton-on-Hudson (lat,lon)

# helper to chunk a vector into batches of size n
chunk_vec <- function(x, n) split(x, ceiling(seq_along(x) / n))
origin_batches <- chunk_vec(origins, 25)

# ——————————————————————————————————————————————————
# 6) function to query one batch, with error‐handling
# ——————————————————————————————————————————————————
get_matrix <- function(orig_chunk, dest) {
  res <- tryCatch(
    google_distance(
      origins      = orig_chunk,
      destinations = dest,
      mode         = "driving"
    ),
    error = function(e) {
      message("  ✖ API error: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(res) || length(res$rows) == 0) {
    # return NAs if the call failed
    return(tibble(
      origin     = orig_chunk,
      distance_m = NA_real_,
      duration_s = NA_real_
    ))
  }
  
  elems <- res$rows$elements
  tibble(
    origin     = orig_chunk,
    distance_m = map_dbl(elems, ~ .x$distance$value),
    duration_s = map_dbl(elems, ~ .x$duration$value)
  )
}

# ——————————————————————————————————————————————————
# 7) loop over batches and combine
# ——————————————————————————————————————————————————
results_list <- map(origin_batches, ~ get_matrix(.x, destination))
results <- bind_rows(results_list)

# ——————————————————————————————————————————————————
# 8) attach town names, convert units, and save
# ——————————————————————————————————————————————————
final_tbl <- towns_df %>%
  select(name) %>%
  bind_cols(results) %>%
  mutate(
    distance_miles   = distance_m / 1609.344,
    duration_minutes = duration_s / 60
  )

saveRDS(final_tbl, "towns_to_croton_google_distances.rds")
