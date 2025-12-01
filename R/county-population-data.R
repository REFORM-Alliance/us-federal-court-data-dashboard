rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(tidycensus)
library(idbr)
library(readxl)


####Set Up####
##API Census Key
# census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = TRUE, install = TRUE)
options(tigris_use_cache = TRUE)

##Read Function
read_census_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(url, tmp, quiet = TRUE, method = "curl")
  fread(tmp) |> clean_names()
}

##Fill SF Geometry (direction = "up")
fill_sf_geometry_up <- function(geom){
  rev_accum <- accumulate(rev(geom), 
                          ~if(st_is_empty(.y) || is.na(st_dimension(.y))){
                            .x
                          }else{
                            .y
                          })
  rev(rev_accum)
}

##Define Year
latest_year <- 
  Sys.Date() %m-% 
  years(1) %>% 
  year()

##Parse Mixed Dates
parse_mixed_dates <- function(x){
  
  parts <- str_match(x, "^(\\d{1,2})/(\\d{1,2})/(\\d{2,4})$")
  month <- as.integer(parts[,2])
  day   <- as.integer(parts[,3])
  year  <- parts[,4]
  
  year <- as.integer(year)
  year <- ifelse(
    nchar(parts[,4]) == 4, 
    year,
    ifelse(
      year <= 24, 2000 + year,  
      1900 + year               
    )
  )
  
  as.Date(sprintf("%04d-%02d-%02d", year, month, day))
}


####County Population Data####
##Set WD Location
la_data_loc <- "/home/basil/implementation/louisiana/incarceration-rates"

##County Population Data 1980-1989
county_populations_1980s <- 
  la_data_loc %>% 
  here("data-raw") %>% 
  # "data-raw" %>% 
  here("e8089co.xls") %>% 
  read_excel(col_names = FALSE) %>% 
  clean_names() %>% 
  rename_with(~.x %>%
                str_replace_all("x", "V")) %>%
  mutate(V1 = 
           V1 %>% 
           replace_na(""),
         header_flag = 
           ifelse(V1 == "FIPS Code", 1, 0) %>% 
           cumsum()) %>% 
  filter(header_flag >= 1) %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if(""))) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  group_split(header_flag, .keep = FALSE) %>% 
  map(~.x %>% 
        row_to_names(1) %>% 
        clean_names() %>% 
        mutate(across(where(is_character), 
                      ~.x %>% 
                        na_if("") %>% 
                        na_if("NA"))) %>% 
        filter(is.na(fips_code) == FALSE) %>% 
        pivot_longer(cols = c(starts_with("estimate_"), starts_with("census_")),
                     names_to = "year", 
                     values_to = "pop_estimate") %>% 
        mutate(year = 
                 year %>% 
                 str_replace_all("estimate_", "") %>% 
                 str_replace_all("census_", "") %>% 
                 as.numeric(),
               pop_estimate = 
                 pop_estimate %>% 
                 as.numeric())) %>% 
  bind_rows() %>% 
  rename("state" = "area_name") %>% 
  arrange(fips_code, year) %>% 
  filter(!(fips_code == "00000")) %>% 
  rename("state_county_name" = "state") %>% 
  mutate(state_county_name = 
           state_county_name %>% 
           str_replace_all("Co[.]$", "County")) %>% 
  mutate(fips_code = 
           ifelse(str_length(fips_code) == 2, paste0(fips_code, "000"), fips_code),
         state_fips_code = 
           fips_code %>% 
           str_sub(1, 2),
         county_fips_code = 
           fips_code %>% 
           str_sub(3, 5), 
         state_name = 
           ifelse(county_fips_code == "000", state_county_name, NA)) %>% 
  group_by(state_fips_code) %>% 
  tidyr::fill(state_name, .direction = "down") %>% 
  ungroup() %>% 
  rename("estimate" = "pop_estimate")

##State Population Data 1990-1999

# url <- "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/totals/99c8_00.txt"
# url %>% 
#   download.file(destfile = here("data-raw", "99c8_00.txt"), mode = "wb")

county_populations_1990s <- 
  "data-raw" %>% 
  here("99c8_00.txt") %>% 
  read.delim(header = FALSE, sep = "") %>% 
  suppressWarnings() %>% 
  dplyr::select(-c(V13)) %>% 
  setNames(c("pop_type", "fips_code", paste0("estimate_", c(1999:1990)), "state_county_name")) %>% 
  clean_names() %>% 
  mutate(filter_flag = 
           ifelse(state_county_name == "Alabama", 1, 0) %>% 
           cummax()) %>% 
  filter(filter_flag == 1) %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if(""))) %>% 
  dplyr::select(-filter_flag) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(rowname = 
           ifelse(str_detect(rowname, "^na") == TRUE, "state_county_name", rowname), 
         across(where(is_character), 
                ~.x %>% 
                  replace_na("")),
         across(starts_with("V"), 
                ~ifelse(str_detect(.x, "V\x82") == TRUE, NA, .x))) %>% 
  tidyr::fill(starts_with("V"), .direction = "up") %>% 
  group_by(rowname) %>% 
  dplyr::summarize(across(where(is_character), 
                          ~.x %>% 
                            paste(collapse = " "))) %>% 
  ungroup() %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  as_tibble() %>% 
  mutate(fips_filter = 
           case_when(str_detect(fips_code, "[:digit:]:") == TRUE ~ 1,
                     state_county_name == "States" ~ 1, 
                     estimate_1990 == "United" ~ 1, 
                     TRUE ~ 0)) %>% 
  filter(fips_filter == 0) %>% 
  mutate(state_county_name = 
           state_county_name %>% 
           str_trim(side = "both") %>% 
           str_replace_all("P. of Wales-Outer Ketchikan[(]1[)]", "Prince of Wales-Outer Ketchikan Census Area")) %>% 
  filter(!(fips_code == "Full")) %>% 
  mutate(state_county_name = 
           ifelse(pop_type == 1, state_county_name, NA)) %>% 
  group_by(fips_code) %>% 
  tidyr::fill(state_county_name, .direction = "down") %>% 
  ungroup() %>% 
  mutate(across(starts_with("estimate_"), 
                ~.x %>% 
                  str_replace_all(",", "") %>% 
                  as.numeric() %>% 
                  suppressWarnings())) %>% 
  filter(pop_type == 1) %>% 
  dplyr::select(fips_code, state_county_name, starts_with("estimate_")) %>% 
  pivot_longer(cols = starts_with("estimate_"),
               names_to = "year",
               values_to = "pop_estimate") %>% 
  mutate(year = 
           year %>% 
           str_replace_all("estimate_", "") %>% 
           as.numeric(),
         pop_estimate = 
           pop_estimate %>% 
           str_replace_all(",", "") %>% 
           as.numeric()) %>% 
  arrange(year) %>% 
  mutate(fips_code = 
           ifelse(str_length(fips_code) == 2, paste0(fips_code, "000"), fips_code),
         state_fips_code = 
           fips_code %>% 
           str_sub(1, 2),
         county_fips_code = 
           fips_code %>% 
           str_sub(3, 5), 
         state_name = 
           ifelse(county_fips_code == "000", state_county_name, NA)) %>% 
  group_by(state_fips_code) %>% 
  tidyr::fill(state_name, .direction = "down") %>% 
  ungroup() %>% 
  rename("estimate" = "pop_estimate")

##County Population Data 2000-2009
county_pop_data_2000s <- 
  county_populations_1990s %>% 
  pull(state_fips_code) %>% 
  unique() %>% 
  map(~paste0("https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/asrh/cc-est2009-alldata-", .x,  ".csv")) %>% 
  unlist() %>% 
  map(~.x %>% 
        read_census_csv() %>% 
        clean_names() %>% 
        filter(agegrp == 0) %>% 
        dplyr::select(state, county, stname, ctyname, year, tot_pop) %>% 
        mutate(county = 
                 county %>% 
                 str_pad(width = 3, side = "left", "0"),
               state = 
                 state %>% 
                 str_pad(width = 2, side = "left", "0"), 
               year = 
                 year %>% 
                 as.numeric(),
               across(where(is_character), 
                      ~.x %>% 
                        na_if("X"))) %>% 
        group_by(state, county, stname, ctyname, year) %>% 
        dplyr::summarize(tot_pop = 
                           tot_pop %>% 
                           as.numeric() %>% 
                           sum(na.rm = TRUE)) %>% 
        ungroup()) %>% 
  bind_rows() %>% 
  rename("state_name" = "stname", 
         "state_county_name" = "ctyname", 
         "estimate" = "tot_pop", 
         "state_fips_code" = "state", 
         "county_fips_code" = "county") %>% 
  mutate(date_clean = 
           (2009 - (max(year))) + year,
         fips_code = 
           paste0(state_fips_code, county_fips_code)) %>% 
  filter(!(year == 1)) %>% 
  mutate(date_clean = 
           case_when(date_clean == 1999 ~ as.Date("2000-04-01"),
                     date_clean != 1999 ~ as.Date(paste0(date_clean, "-07-01")))) %>% 
  dplyr::select(-year) %>% 
  filter(month(date_clean) == 7) %>% 
  mutate(year = 
           date_clean %>% 
           year()) %>%
  dplyr::select(-date_clean)

##County Population Data 2010-2019
county_pop_data_2010s <- 
  get_estimates(
    geography = "county", 
    product = "population", 
    time_series = TRUE, 
    geometry = TRUE, 
    vintage = 2019
  ) %>% 
  clean_names() %>% 
  filter(variable == "POP") %>% 
  dplyr::select(geoid, name, date, value, geometry) %>% 
  rename("fips_code" = "geoid", 
         "state_county_name" = "name") %>% 
  separate_wider_delim(cols = "state_county_name", delim = ", ", names = c("county_name", "state_name"), cols_remove = FALSE) %>% 
  mutate(state_fips_code = 
           fips_code %>% 
           str_sub(1, 2),
         county_fips_code = 
           fips_code %>% 
           str_sub(3, 5), 
         date_clean = 
           (2019 - (max(date))) + date) %>% 
  filter(!(date == 1)) %>% 
  mutate(date_clean = 
           case_when(date_clean == 2009 ~ as.Date("2010-04-01"),
                     date_clean != 2009 ~ as.Date(paste0(date_clean, "-07-01")))) %>% 
  dplyr::select(-date) %>% 
  filter(month(date_clean) == 7) %>% 
  mutate(year = 
           date_clean %>% 
           year()) %>% 
  dplyr::select(-c(date_clean, state_county_name)) %>% 
  rename("estimate" = "value", 
         "state_county_name" = "county_name")

##County Population Data 2020-latest_year
county_pop_data_2020s <-
  get_estimates(
    geography = "county",
    product = "population",
    time_series = TRUE,
    geometry = TRUE,
    vintage = latest_year) %>% 
  clean_names() %>% 
  filter(variable == "POPESTIMATE") %>% 
  dplyr::select(geoid, name, year, value, geometry) %>% 
  rename("fips_code" = "geoid", 
         "state_county_name" = "name") %>% 
  separate_wider_delim(cols = "state_county_name", delim = ", ", names = c("county_name", "state_name"), cols_remove = FALSE) %>% 
  mutate(state_fips_code = 
           fips_code %>% 
           str_sub(1, 2),
         county_fips_code = 
           fips_code %>% 
           str_sub(3, 5)) %>% 
  dplyr::select(-c(state_county_name)) %>% 
  rename("estimate" = "value", 
         "state_county_name" = "county_name")

##Combine Population Data
county_population_data_full <- 
  county_populations_1980s %>% 
  bind_rows(county_populations_1990s) %>% 
  bind_rows(county_pop_data_2000s) %>%  
  bind_rows(county_pop_data_2010s) %>% 
  bind_rows(county_pop_data_2020s) %>% 
  filter(!(county_fips_code == "000")) %>%
  st_as_sf() %>% 
  mutate(state_county_name = 
           state_county_name %>% 
           str_trim(side = "both") %>% 
           str_replace_all("Prin Wales-Outer Ketch C[.]A[.]", "Prince of Wales-Outer Ketchikan Census Area") %>% 
           str_replace_all("Skagway-Yakutat-Angoon C[.]A[.]", "Skagway-Hoonah-Angoon Census Area") %>% 
           str_replace_all("Wade Hampton C[.]A[.]", "Wade Hampton Census Area") %>% 
           str_replace_all("Wade Hampton C[.]A", "Wade Hampton Census Area") %>% 
           str_replace_all("Wrangell-Petersburg C[.]A[.]", "Wrangell-Petersburg Census Area") %>% 
           str_replace_all("Ketchikan Gateway Bor[.]", "Ketchikan Gateway Borough") %>% 
           str_replace_all("C[.]A[.]", "Census Area") %>% 
           str_replace_all("Bor[.]", "Borough") %>% 
           str_replace_all("Par[.]", "Parish"),
         state_county_name = 
           case_when(state_county_name == "Shannon County" & state_name == "South Dakota" ~ "Oglala Lakota County",
                     state_county_name == "Clifton Forge city" & state_name == "Virginia" ~ "Alleghany County", 
                     state_county_name == "South Boston city" & state_name == "Virginia" ~ "Halifax County", 
                     state_county_name == "Bedford city" & state_name == "Virginia" ~ "Bedford County", 
                     state_county_name == "Dade County" & state_name == "Florida" ~ "Miami-Dade County",
                     TRUE ~ state_county_name),
         fips_code = 
           case_when(fips_code == "46113" ~ "46102",
                     fips_code == "51560" ~ "51005",
                     fips_code == "51780" ~ "51083",
                     fips_code == "51515" ~ "51019",
                     fips_code == "12025" ~ "12086",
                     TRUE ~ fips_code),
         state_fips_code = 
           fips_code %>% 
           str_sub(1, 2),
         county_fips_code = 
           fips_code %>% 
           str_sub(3, 5)) %>% 
  arrange(fips_code, year) %>% 
  group_by(fips_code) %>%
  mutate(geometry = 
           geometry %>% 
           st_make_valid() %>% 
           fill_sf_geometry_up()) %>%
  ungroup() %>% 
  group_by(fips_code) %>% 
  mutate(n = 
           state_county_name %>% 
           n_distinct(na.rm = TRUE), 
         max_year = 
           year %>% 
           max(na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(clean_state_county_name = 
           ifelse(year == max_year, state_county_name, NA)) %>% 
  group_by(fips_code) %>% 
  tidyr::fill(clean_state_county_name, .direction = "up") %>% 
  ungroup() %>% 
  mutate(state_county_name = 
           ifelse(n <= 1, state_county_name, clean_state_county_name)) %>% 
  dplyr::select(-c(n, max_year, clean_state_county_name))

##IDB Island Populations 
idb_api_key(Sys.getenv("CENSUS_API_KEY"))
islands <- c("AS", "VI", "PR", "GU", "MP")
idb_pops <- 
  islands %>% 
  map(~.x %>% 
        get_idb(year = c(1980:latest_year), 
                variables = "pop")) %>% 
  bind_rows() %>% 
  rename("state_abbr" = "code", 
         "state_district" = "name") %>% 
  mutate(state_district = 
           state_district %>% 
           str_replace_all(", U[.]S[.]", "") %>% 
           str_trim(side = "both"))
  

####Convert Counties to Districts####
##Read in Judicial District
judicial_district_sf <- 
  "data" %>% 
  here("judicial_sf_data.rds") %>% 
  read_rds() 

##Read in Judicial District to County Crosswalk
judicial_district_to_county <- 
  "data-raw" %>% 
  here("judicial_district_counties_crosswalk.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  dplyr::select(statename, county, district, fips, end_district) %>% 
  distinct() %>% 
  rename("state_name" = "statename", 
         "fips_code" = "fips") %>%  
  mutate(fips_code = 
           fips_code %>% 
           str_pad(width = 5, side = "left", pad = "0"),
         state_name = 
           state_name %>% 
           str_to_title(), 
         district = 
           district %>% 
           str_replace_all(" District$", "") %>% 
           str_trim(side = "both"), 
         county = 
           case_when(state_name == "South Carolina" ~ "ALL", 
                     TRUE ~ county)) %>% 
  mutate(district = 
           case_when(county == "ALL" ~ state_name,
                     TRUE ~ paste0(state_name, " ", district)),
         end_district = 
           end_district %>% 
           parse_mixed_dates()) %>% 
  group_by(fips_code) %>% 
  filter(end_district == max(end_district, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(district, fips_code) %>% 
  distinct()

##Create Crosswalk
counties_with_district_crosswalk <- 
  county_population_data_full %>% 
  st_drop_geometry() %>% 
  left_join(judicial_district_to_county, by = "fips_code") %>% 
  mutate(district = 
           case_when(is.na(district) == TRUE & state_name %in% c("Alaska", "Connecticut", "South Dakota") ~ state_name,
                     TRUE ~ district))

##Judicial District Populations
judicial_district_pops <- 
  judicial_district_sf %>% 
  st_drop_geometry() %>% 
  pull(state_district) %>% 
  expand_grid(state_district = ., year = c(1980:latest_year)) %>% 
  left_join(judicial_district_sf %>% 
              st_drop_geometry() %>% 
              dplyr::select(objectid, state_district) %>% 
              rename("district_id" = "objectid"), 
            by = "state_district") %>%
  left_join(counties_with_district_crosswalk %>% 
              group_by(district, year) %>% 
              dplyr::summarize(estimate = 
                                 estimate %>% 
                                 sum(na.rm = TRUE)) %>% 
              ungroup() %>% 
              rename("state_district" = "district") %>% 
              mutate(state_district = 
                       state_district %>% 
                       str_replace_all(" Of ", " of ")), 
            by = c("state_district", "year")) %>% 
  left_join(idb_pops %>% 
              dplyr::select(-state_abbr), 
            by = c("year", "state_district")) %>% 
  mutate(estimate = 
           ifelse(is.na(estimate) == TRUE, pop, estimate)) %>% 
  dplyr::select(-pop)


####Write CSV####
##Write Judicial District Populations
fwrite(judicial_district_pops, here("data", "judicial_district_pops.csv"), row.names = FALSE)






# counties_with_district <- 
#   county_population_data_full %>% 
#   st_make_valid() %>% 
#   st_set_crs(st_crs(judicial_district_sf)) %>% 
#   mutate(county_area = 
#            geometry %>% 
#            st_area()) %>% 
#   st_intersection(judicial_district_sf %>% 
#                     st_make_valid()) %>% 
#   mutate(intersection_area = 
#            geometry %>% 
#            st_area(), 
#          pct_of_county = intersection_area/county_area)
# 
# 
# counties_with_district <- 
#   county_population_data_full %>% 
#   st_buffer(dist = 0.0001) %>% 
#   st_make_valid() %>% 
#   st_set_crs(st_crs(judicial_district_sf)) %>% 
#   st_join(judicial_district_sf %>% 
#             st_make_valid(),
#           join = st_intersects)










