cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

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
library(osrm)

# usethis::edit_r_environ()
# mapbox_public_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
# mapbox_local_token  <- Sys.getenv("MAPBOX_TOKEN_LOCAL")
# Sys.setenv(MAPBOX_TOKEN = mapbox_token)
# mapbox_token  <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

# ---- Data Preparation ----n# Ensure caching of tigris shapes\options(tigris_use_cache = TRUE)

# MCAS data
mcas <- read_csv("data/MCAS_Achievement_Results_20250415.csv") %>%
  filter(
    SY == 2024,
    STUGRP == "White",
    ORG_NAME != "State",
    TEST_GRADE == "10",
    ORG_TYPE == "Public School District"
  ) %>%
  select(SY:STUGRP, STU_CNT, E_CNT, AVG_SCALED_SCORE)

mcas_agg <- mcas %>%
  group_by(DIST_CODE, DIST_NAME) %>%
  summarize(
    stu_cnt         = median(STU_CNT),
    exceed_cnt      = round(weighted.mean(E_CNT, w = STU_CNT), 0),
    avg_score       = round(weighted.mean(AVG_SCALED_SCORE, w = STU_CNT), 0)
  ) %>%
  ungroup() %>%
  mutate(
    exceed_perct            = round(exceed_cnt / stu_cnt, 3),
    school_size_est         = stu_cnt * 4
    # exceed_mcas_percentile  = percent_rank(exceed_perct)
  )

ap_scores <- read_csv("data/Advanced_Placement__AP__Performance_20250525.csv") %>%
  filter(ORG_TYPE == "District", SUBJ_CAT == "All Subjects", STU_GRP == "All Students", SY == "2024") %>%
  select(SY, DIST_CODE, PCT_3_5)
# mutate(passing_ap_perctile = percent_rank(PCT_3_5))

# School district crosswalk
town_school_dist_xwalk <- read_csv("data/final_school_districts_mapping_v1.csv") %>%
  distinct() %>%
  arrange(town_name) %>%
  group_by(town_name) %>%
  slice(1)

# Zillow three-bedroom price change
price_town_mapping <- read_csv("data/Mapping_Table_with_Exact_Match_Column2.csv")
three_bed_home_price_zil <- read_csv("data/City_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") %>%
  filter(State == "MA") %>%
  select(RegionName, last_col(offset = 12), last_col()) %>%
  mutate(one_year_price_change = round((`3/31/2025` - `3/31/2024`) / `3/31/2024` * 100, 1)) %>%
  rename(
    current_typ_home_value = `3/31/2025`,
    lst_yr_typ_home_value = `3/31/2024`
  )

# Add property tax rates
prop_tax_rates <- read_xlsx("data/taxratesbyclass.xlsx") %>% 
  mutate(
    prop_rate = Residential / 1000
  ) %>% 
  select(Municipality, prop_rate)

# Town geometry and joins
towns_sf <- tigris::county_subdivisions(state = "MA", cb = TRUE, year = 2023) %>%
  rename(town_name = NAME) %>%
  left_join(town_school_dist_xwalk, by = "town_name") %>%
  left_join(mcas_agg, by = c("DIST_NAME")) %>%
  left_join(price_town_mapping, by = "town_name") %>%
  left_join(three_bed_home_price_zil, by = c("region_name" = "RegionName")) %>%
  left_join(ap_scores, by = "DIST_CODE") %>%
  mutate(
    muni_id = str_replace(town_name,".Town$", ""),
    muni_id = if_else(town_name == "Manchester-by-the-Sea", "Manchester By The Sea", muni_id)
  ) %>% 
  left_join(prop_tax_rates %>% select(Municipality, prop_rate) , by = c("muni_id" = "Municipality")) %>%
  st_transform(4326)

# Croton geometry
# 1. Define Croton as a POINT (lon, lat) in the same CRS as your towns_sf:
croton_pt <- st_sfc(
  st_point(c(-73.891743, 41.218139)),
  crs = 4326
)

st_crs(towns_sf)
#> Coordinate Reference System:
#>   EPSG:4326
#>   proj4string: "+proj=longlat +datum=WGS84 +no_defs"

st_crs(croton_pt)
#> Coordinate Reference System:
#>   EPSG:4326
#>   proj4string: "+proj=longlat +datum=WGS84 +no_defs"

# 2. Compute each town’s centroid and distance to Croton (in km):
towns_sf <- towns_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    dist_to_croton_mi =
      round(as.numeric(
        st_distance(centroid, croton_pt)
      ) / 1000 / 1.60934)
  ) %>%
  mutate(
    dist_to_croton_mi = dist_to_croton_mi + 50,
    mcas_rank = percent_rank(exceed_perct),
    ap_rank = percent_rank(PCT_3_5),
    normalized_school_score = round((.5 * mcas_rank + .5 * ap_rank) * 100, 1),
    school_color = if_else(normalized_school_score >= 70, 1, 0),
    tier_2_color = if_else(normalized_school_score > 50 & normalized_school_score < 69, 1, 0),
    fill_color = case_when(
      tier_2_color ==1 ~ "#AB47BC",
      school_color == 1 ~ "#009688",
      TRUE ~ "transparent"
    )
  )

###############################
# Calculate driving distances #
###############################

# 2. swap in your 'centroid' column as the active geometry
towns_centroids <- st_set_geometry(towns_sf, "centroid")

# 3a. get driving DISTANCES (meters)
tbl_dist <- osrmTable(
  src     = towns_centroids,
  dst     = croton_pt,
  measure = "distance"
)

# 4. pull out the one‐column vectors
towns_sf$dist_m <- tbl_dist$distances[, 1]
towns_sf$dist_mi <- towns_sf$dist_m / 1609.34 # meters to miles

towns_sf <- readRDS("data/towns_sf.rds") |>
  st_transform(4326) |>
  st_simplify(dTolerance = 100) |>
  st_make_valid()

# Build popup HTML and keep only needed columns
towns_map <- towns_sf |>
  mutate(
    popup_html = paste0(
      "<strong>Town:</strong> ", town_name, "<br/>",
      "<strong>School District:</strong> ", DIST_NAME, "<br/>",
      "<strong>Home Price (3 bed):</strong> $", round(current_typ_home_value / 1000), "K<br/>",
      "<strong>Property Tax Rate: </strong>", percent(prop_rate, accuracy = .01), "<br/>",
      "<strong>High School Size:</strong> ", school_size_est, "<br/>",
      "<strong>School Rating:</strong> ",
      ifelse(is.na(normalized_school_score), "NA", paste0(normalized_school_score, "%")), "<br/>",
      "<strong>To Croton (NY):</strong> ",
      paste0(round(dist_mi), " miles (", round((dist_mi / 65) * 60), " min)")
    )
  ) |>
  mutate(across(where(is.factor), as.character)) |>
  select(town_name, fill_color, popup_html, geometry)

saveRDS(towns_map, "data/towns_map.rds")
saveRDS(towns_sf, "data/towns_sf.rds")
