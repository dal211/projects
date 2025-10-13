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
library(glue)

# usethis::edit_r_environ()
# mapbox_public_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
# mapbox_local_token  <- Sys.getenv("MAPBOX_TOKEN_LOCAL")
# Sys.setenv(MAPBOX_TOKEN = mapbox_token)
# mapbox_token  <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

# ---- Data Preparation ----n# Ensure caching of tigris shapes\options(tigris_use_cache = TRUE)

pop_density <- readRDS("data/pop_density.rds")
pop_density <- as_tibble(pop_density)

# MCAS data
mcas <- read_csv("data/MCAS_Achievement_Results_20250415.csv") %>%
  filter(
    SY == 2024,
    STUGRP == "White",
    ORG_NAME != "State",
    TEST_GRADE == "10",
    ORG_TYPE == "Public School District"
  ) %>%
  select(SY:STUGRP, STU_CNT, E_CNT, AVG_SCALED_SCORE, contains("SGP"))

mcas_agg <- mcas %>%
  group_by(DIST_CODE, DIST_NAME) %>%
  summarize(
    # cohort size across grades/subjects
    tested_total = sum(STU_CNT, na.rm = TRUE),
    
    # achievement: weight by tested count
    avg_score = weighted.mean(AVG_SCALED_SCORE, w = STU_CNT, na.rm = TRUE),
    
    # growth: weight by students included in SGP
    sgp_included = sum(AVG_SGP_INCL, na.rm = TRUE),
    avg_sgp = ifelse(
      sgp_included > 0,
      weighted.mean(AVG_SGP, w = AVG_SGP_INCL, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    school_size_est = round(tested_total / 4)  # or use your preferred assumption
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
    sgp_outlook = case_when(
      is.na(avg_sgp) ~ "Growth: not available",
      is.na(sgp_included) ~ "Growth: not available",
      sgp_included < 20 ~ "Growth: insufficient data",
      avg_sgp >= 60 ~ "Growth: High",
      avg_sgp <= 39 ~ "Growth: Low",
      TRUE ~ "Growth: Typical"
    ),
    # optional icon flair
    sgp_outlook_icon = case_when(
      sgp_outlook == "Growth: High" ~ "⬆️",
      sgp_outlook == "Growth: Low" ~ "⬇️",
      sgp_outlook == "Growth: Typical" ~ "➡️",
      TRUE ~ "—"
    ),
  ) %>% 
  mutate(
    dist_to_croton_mi = dist_to_croton_mi + 50,
    mcas_rank = percent_rank(avg_score),
    ap_rank = percent_rank(PCT_3_5),
    sgp_rank = percent_rank(avg_sgp),
    normalized_school_score = round((.6 * mcas_rank + .4 * ap_rank) * 100, 1),
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

towns_sf <- towns_sf |>
  st_transform(4326) |>
  st_simplify(dTolerance = 100) |>
  st_make_valid()

towns_sf <- towns_sf %>% 
  left_join(
    pop_density,
    by = c("town_name" = "town_clean")
  ) %>% 
  mutate(density = round(density))

towns_map <- towns_sf %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(
    popup_html = glue::glue(
      "<strong>Town:</strong> {town_name}<br/>
       <strong>Area Feel:</strong> {dens_cat} ({comma(density)} per sq mi)<br/>
       <strong>School District:</strong> {DIST_NAME}<br/>
       <strong>Median Home Price (3 bed):</strong> {dollar(current_typ_home_value, accuracy = 1)}<br/>
       <strong>Property Tax Rate:</strong> {percent(prop_rate, accuracy = 0.01)}<br/>
       <strong>High School Size Est.:</strong> {comma(school_size_est)}<br/>
       <strong>School Rating:</strong> {ifelse(is.na(normalized_school_score), 'NA', paste0(round(normalized_school_score, 1), '%'))} (overall); 
       {percent(mcas_rank, accuracy = 1)} (MCAS); 
       {percent(ap_rank,   accuracy = 1)} (AP)<br/>
       <strong>School Outlook:</strong> {sgp_outlook}{ifelse(is.na(avg_sgp), '', paste0(' (', round(avg_sgp, 0), ')'))}<br/>
       <strong>To Croton (NY):</strong> {round(dist_mi)} miles ({round(dist_mi / 65 * 60)} min)"
    ) |>
      as.character()
  ) %>%
  select(town_name, fill_color, popup_html, geometry)

saveRDS(towns_map, "data/towns_map.rds")
saveRDS(towns_sf, "data/towns_sf.rds")


