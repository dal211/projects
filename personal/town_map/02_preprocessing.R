cat("===== LOADED UPDATED APP.R @", Sys.time(), "=====\n")

library(dplyr)
library(tigris)
library(readr)
library(sf)
library(tidyverse)
library(scales)
library(readxl)
library(glue)
library(httr)
library(jsonlite)

# ---- Data Preparation ----
# Ensure caching of tigris shapes
options(tigris_use_cache = TRUE)

pop_density <- readRDS("data/pop_density.rds")
pop_density <- as_tibble(pop_density)

# MCAS data
mcas_endpoint <- "https://educationtocareer.data.mass.gov/resource/i9w6-niyt.json"

latest_mcas_sy <- httr::GET(
  mcas_endpoint,
  query = list(`$select` = "max(sy) as max_sy")
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  pull(max_sy)

mcas <- httr::GET(
  mcas_endpoint,
  query = list(
    `$select` = "sy,dist_code,dist_name,test_grade,stu_grp,stu_cnt,e_cnt,avg_scaled_score",
    `$where`  = glue::glue("org_type='Public School District' AND test_grade='10' AND stu_grp='All Students' AND sy='{latest_mcas_sy}'"),
    `$limit`  = 50000
  )
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  transmute(
    SY = sy,
    DIST_CODE = dist_code,
    DIST_NAME = dist_name,
    STUGRP = stu_grp,
    STU_CNT = as.numeric(stu_cnt),
    E_CNT = as.numeric(e_cnt),
    AVG_SCALED_SCORE = as.numeric(avg_scaled_score)
  )

mcas_agg <- mcas %>%
  group_by(DIST_CODE, DIST_NAME) %>%
  summarize(
    # cohort size across grades/subjects
    stu_cnt = median(STU_CNT),

    # achievement: weight by tested count
    avg_score = weighted.mean(AVG_SCALED_SCORE, w = STU_CNT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    school_size_est = stu_cnt * 4
  )

ap_endpoint <- "https://educationtocareer.data.mass.gov/resource/787a-3wen.json"

latest_ap_sy <- httr::GET(
  ap_endpoint,
  query = list(`$select` = "max(sy) as max_sy")
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  pull(max_sy)

ap_scores <- httr::GET(
  ap_endpoint,
  query = list(
    `$select` = "sy,dist_code,pct_3_5,tests_taken",
    `$where`  = glue::glue("org_type='District' AND subj_cat='All Subjects' AND stu_grp='All Students' AND sy='{latest_ap_sy}'"),
    `$limit`  = 50000
  )
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  transmute(
    SY = sy,
    DIST_CODE = dist_code,
    PCT_3_5 = as.numeric(pct_3_5),
    TESTS_TAKEN = as.numeric(tests_taken)
  )

# SAT data
sat_endpoint <- "https://educationtocareer.data.mass.gov/resource/wihy-jkek.json"

latest_sat_sy <- httr::GET(
  sat_endpoint,
  query = list(`$select` = "max(sy) as max_sy")
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  pull(max_sy)

sat_scores <- httr::GET(
  sat_endpoint,
  query = list(
    `$select` = "sy,dist_code,taken_cnt,read_write_score,math_score",
    `$where`  = glue::glue("org_type='District' AND stu_grp='All Students' AND sy='{latest_sat_sy}'"),
    `$limit`  = 50000
  )
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  transmute(
    SY = sy,
    DIST_CODE = dist_code,
    TAKEN_CNT = as.numeric(taken_cnt),
    SAT_TOTAL = as.numeric(read_write_score) + as.numeric(math_score)
  )

# College enrollment outcomes (immediate fall enrollment, latest grad cohort)
# DART reports by school, so sum graduates and enrollees up to the district
college_endpoint <- "https://educationtocareer.data.mass.gov/resource/adqe-6sht.json"
college_indicator <- "Students enrolled in postsecondary education in the immediate fall after high school graduation"

latest_college_sy <- httr::GET(
  college_endpoint,
  query = list(
    `$select` = "max(sy) as max_sy",
    `$where`  = glue::glue("indicator='{college_indicator}'")
  )
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  pull(max_sy)

college_outcomes <- httr::GET(
  college_endpoint,
  query = list(
    `$select` = "dist_code,sum(stu_cnt) as enrolled,sum(stu_incl) as grads",
    `$where`  = glue::glue("indicator='{college_indicator}' AND stu_grp='All Students' AND sy='{latest_college_sy}' AND dist_code!='00000000'"),
    `$group`  = "dist_code",
    `$limit`  = 50000
  )
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  transmute(
    DIST_CODE = dist_code,
    college_bound_rate = as.numeric(enrolled) / as.numeric(grads)
  )

# School district crosswalk
town_school_dist_xwalk <- read_csv("data/final_school_districts_mapping_v1.csv") %>%
  distinct() %>%
  arrange(town_name) %>%
  group_by(town_name) %>%
  slice(1)

# Zillow three-bedroom price change
price_town_mapping <- as.data.frame(readr::read_csv("data/price_town_mapping.csv", lazy = FALSE, show_col_types = FALSE))

# Zillow updates this file monthly; latest month vs. same month a year earlier
zillow_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/City_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

three_bed_home_price_zil <- readr::read_csv(zillow_url, show_col_types = FALSE) %>%
  filter(State == "MA") %>%
  select(RegionName, lst_yr_typ_home_value = last_col(offset = 12), current_typ_home_value = last_col()) %>%
  mutate(one_year_price_change = round((current_typ_home_value - lst_yr_typ_home_value) / lst_yr_typ_home_value * 100, 1))

# Add property tax rates
# MA DLS "Tax Rates by Class" Excel export; towns set rates through the fiscal
# year (starts July 1), so pull the last 5 FYs and keep each town's latest rate
dls_tax_url <- "https://dls-gw.dor.state.ma.us/reports/rdPage.aspx?rdReport=PropertyTaxInformation.taxratesbyclass.taxratesbyclass&rdReportFormat=NativeExcel&rdExportTableID=tbl_taxratesbyclass&rdExcelOutputFormat=Excel2007"
current_fy <- as.integer(format(Sys.Date() + 184, "%Y"))
tax_xlsx <- tempfile(fileext = ".xlsx")

invisible(httr::POST(
  dls_tax_url,
  body = setNames(as.list(current_fy - 0:4), rep("iclYear", 5)),
  encode = "form",
  httr::write_disk(tax_xlsx, overwrite = TRUE)
))

prop_tax_rates <- read_xlsx(tax_xlsx) %>%
  filter(Residential > 0) %>%
  slice_max(`Fiscal Year`, by = Municipality) %>%
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
  left_join(sat_scores, by = "DIST_CODE") %>%
  left_join(college_outcomes, by = "DIST_CODE") %>%
  mutate(
    muni_id = str_replace(town_name, ".Town$", ""),
    muni_id = if_else(town_name == "Manchester-by-the-Sea", "Manchester By The Sea", muni_id)
  ) %>%
  left_join(prop_tax_rates %>% select(Municipality, prop_rate), by = c("muni_id" = "Municipality")) %>%
  st_transform(4326)

towns_sf <- towns_sf %>%
  mutate(
    mcas_rank = percent_rank(avg_score),
    ap_rank = percent_rank(PCT_3_5),
    sat_rank = percent_rank(SAT_TOTAL),
    normalized_school_score = round((mcas_rank + ap_rank + sat_rank)/(3) * 100, 1),
    school_color = if_else(normalized_school_score >= 70, 1, 0),
    tier_2_color = if_else(normalized_school_score > 50 & normalized_school_score < 69, 1, 0),
    fill_color = case_when(
      tier_2_color == 1 ~ "#AB47BC",
      school_color == 1 ~ "#009688",
      TRUE ~ "transparent"
    )
  )

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
      "<div class='tp'>
         <div class='tp-head'><span class='tp-dot' style='background:{ifelse(fill_color == \"transparent\", \"#9aa5b1\", fill_color)}'></span>{town_name}</div>
         <div class='tp-sub'>{dens_cat} · {comma(density)}/sq mi · {DIST_NAME} schools</div>
         <div class='tp-grid'>
           <span class='tp-l'>Price</span><span class='tp-v'>{
             ifelse(is.na(current_typ_home_value),
                    'NA',
                    scales::label_number(accuracy = 1, scale = 1/1000, suffix = 'k', big.mark = '')(current_typ_home_value))
           }</span>
           <span class='tp-l'>Tax</span><span class='tp-v'>{percent(prop_rate, accuracy = 0.01)}</span>
           <span class='tp-l'>HS size</span><span class='tp-v'>{comma(school_size_est)}</span>
           <span class='tp-l'>College</span><span class='tp-v'>{ifelse(is.na(college_bound_rate), 'NA', percent(college_bound_rate, accuracy = 1))}</span>
         </div>
         <div class='tp-rating'>
           <div class='tp-score'>School score <span class='tp-pct'>{ifelse(is.na(normalized_school_score), 'NA', paste0(round(normalized_school_score, 1), '%'))}</span></div>
           <div class='tp-breakdown'>MCAS {percent(mcas_rank, accuracy = 1)} · AP {percent(ap_rank, accuracy = 1)} · SAT {percent(sat_rank, accuracy = 1)}</div>
         </div>
       </div>"
    ) |>
      as.character()
  ) %>%
  select(town_name, fill_color, popup_html, geometry)

saveRDS(towns_map, "data/towns_map.rds")
saveRDS(towns_sf, "data/towns_sf.rds")
