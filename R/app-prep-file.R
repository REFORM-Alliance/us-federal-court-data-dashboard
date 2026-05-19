rm(list = ls())

####Read in Libraries####
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(sf)
library(tigris)
library(lubridate)
library(rmapshaper)

####Read in Data####
##Federal Courts Data
federal_courts_df <- 
  "data" %>% 
  here("us_federal_courts_data.csv.gz") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  filter(!(table_num == "E-1" & variables == "persons_under_supervision" & table_name_date != date)) %>% 
  mutate(date_full = 
           ifelse(is.na(date) == TRUE, as.character(table_name_date), as.character(date)) %>% 
           as.Date(),
         date_month = 
           date_full %>% month() %>% as.numeric(), 
         year = 
           date_full %>% year() %>% as.numeric(),
         date_quarter = 
           case_when(date_month == 3 ~ 1,
                     date_month == 6 ~ 2,
                     date_month == 9 ~ 3,
                     date_month == 12 ~ 4, 
                     TRUE ~ NA),
         state_or_district = 
           state_or_district %>% 
           str_replace_all("[.]$", "") %>% 
           str_replace_all(",I$", "")) %>% 
  separate_wider_delim(state_or_district, names = c("state_abb", "district_abb"), 
                       delim = ",", cols_remove = FALSE, too_few = "align_start") %>% 
  left_join(data.frame(state_name = state.name, state_abb = state.abb) %>% 
              bind_rows(data.frame(
                state_name = c("District of Columbia", "Puerto Rico", "Guam", 
                               "Virgin Islands", "Northern Mariana Islands"), 
                state_abb  = c("DC", "PR", "GU", "VI", "MP"))), 
            by = "state_abb") %>% 
  mutate(district_name = 
           case_when(district_abb == "N" ~ "North", 
                     district_abb == "S" ~ "South", 
                     district_abb == "W" ~ "West", 
                     district_abb == "E" ~ "East", 
                     district_abb == "M" ~ "Middle", 
                     district_abb == "C" ~ "Central", 
                     TRUE ~ NA),
         full_district_name = 
           ifelse(is.na(district_name), state_name, paste(state_name, district_name, sep = " ")),
         full_district_name = 
           full_district_name %>% 
           str_replace_all("^North ", "Northern ") %>% 
           str_replace_all("^South ", "Southern ") %>% 
           str_replace_all("^East ",  "Eastern ")  %>% 
           str_replace_all("^West ",  "Western ")  %>%
           str_replace_all("South$",  "Southern")  %>% 
           str_replace_all("West$",   "Western")   %>% 
           str_replace_all("East$",   "Eastern")   %>% 
           str_replace_all("North$",  "Northern")  %>% 
           str_replace_all("Western Virginia",      "West Virginia") %>% 
           str_replace_all("Southern Dakota",       "South Dakota")  %>% 
           str_replace_all("Northern Dakota",       "North Dakota")  %>% 
           str_replace_all("Southern Carolina",     "South Carolina") %>% 
           str_replace_all("Northern Carolina",     "North Carolina") %>% 
           str_replace_all("North Mariana Islands", "Northern Mariana Islands")) %>% 
  dplyr::select(-c(state_abb, district_abb, district_name)) %>% 
  mutate(quarter_name_full = paste0(year, ", Q", date_quarter)) %>% 
  rename("state_district" = "full_district_name") %>% 
  mutate(circuit_and_district = ifelse(state_or_district == "DC", "DC", circuit_and_district)) %>% 
  rename("circuit" = "circuit_and_district") %>% 
  filter(!(variables == "persons_under_supervision" & table_num == "E-1"))

##State Geometries
states_sf <- 
  states(cb = TRUE) %>%
  st_transform(4326) %>% 
  clean_names() %>% 
  rename("state_name" = "name") %>% 
  mutate(state_name =
           state_name %>% 
           str_replace_all("United States Virgin Islands", "Virgin Islands") %>% 
           str_replace_all("Commonwealth of the Northern Mariana Islands", "Northern Mariana Islands")) %>% 
  filter(state_name != "American Samoa")

##Judicial Districts Geometries
judicial_district_sf <- 
  "data" %>% 
  here("judicial_sf_data.rds") %>% 
  read_rds() %>%
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

##Judicial Circuits Geometries
judicial_circuit_sf <- 
  "data" %>% 
  here("judicial_circuit_sf.rds") %>% 
  read_rds() %>% 
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

##Judicial District Populations
judicial_district_pops <- 
  "data" %>% 
  here("judicial_district_pops.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  right_join(federal_courts_df %>% 
               dplyr::select(state_or_district, state_district, circuit) %>% 
               distinct(), 
             by = c("state_district")) %>% 
  dplyr::select(-district_id) %>% 
  {
    df <- .
    df %>%
      bind_rows(df %>%
                  group_by(circuit, year) %>%
                  dplyr::summarize(estimate = estimate %>% sum(na.rm = TRUE)) %>%
                  ungroup() %>%
                  mutate(state_or_district = circuit))
  } %>%
  {
    df <- .
    df %>%
      bind_rows(df %>% 
                  filter(!is.na(state_district)) %>% 
                  group_by(year) %>% 
                  dplyr::summarize(estimate = estimate %>% sum(na.rm = TRUE)) %>% 
                  ungroup() %>% 
                  mutate(state_or_district = "TOTAL"))
  } %>% 
  filter(!is.na(year))

##Add Populations
federal_courts_df_new <- 
  federal_courts_df %>% 
  mutate(state_or_district = state_or_district %>% replace_na("99999")) %>% 
  left_join(judicial_district_pops %>% 
              dplyr::select(year, estimate, state_or_district) %>% 
              distinct() %>% 
              rename("population" = "estimate") %>% 
              mutate(state_or_district = state_or_district %>% replace_na("99999")), 
            by = c("year", "state_or_district")) %>% 
  arrange(state_or_district, year) %>% 
  group_by(state_or_district) %>% 
  tidyr::fill(population, .direction = "down") %>% 
  ungroup() %>% 
  mutate(state_or_district  = state_or_district %>% na_if("99999"), 
         values_per_capita  = 10000 * (values / population),
         state_or_district  = state_or_district %>% str_replace_all("^TOTAL$", "Total"), 
         circuit            = ifelse(state_or_district == "Total", "Total", circuit)) 

##Remove Bad Numbers
bad_numbers <-
  federal_courts_df_new %>%
  filter(!is.na(state_name)) %>%
  mutate(circuit_num =
           circuit %>%
           str_replace_all("th$|nd$|st$|rd$", "") %>%
           as.numeric() %>%
           suppressWarnings(),
         circuit_num = ifelse(is.na(circuit_num), 0, circuit_num)) %>%
  arrange(circuit_num) %>%
  relocate(values, .after = "state_or_district") %>%
  group_by(quarter_name_full, categories, variables_simple) %>%
  dplyr::summarize(count = values %>% sum(na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(federal_courts_df %>%
              filter(state_or_district == "TOTAL") %>%
              dplyr::select(quarter_name_full, categories, variables_simple, values) %>%
              group_by(quarter_name_full, categories, variables_simple) %>%
              dplyr::summarize(values = values %>% sum(na.rm = TRUE)) %>%
              ungroup(),
            by = c("quarter_name_full", "categories", "variables_simple")) %>%
  mutate(same       = ifelse(count == values, TRUE, FALSE),
         diff       = values - count,
         perc_diff  = 100 * (abs(diff) / count),
         same_clean = case_when(same == FALSE & perc_diff <= 5 ~ TRUE,
                                same == FALSE & perc_diff >  5 ~ FALSE,
                                same == TRUE                   ~ TRUE),
         year       = quarter_name_full %>% str_sub(1, 4) %>% as.numeric())

federal_courts_df_final <- 
  federal_courts_df_new %>% 
  anti_join(bad_numbers %>% 
              filter(same_clean == FALSE) %>% 
              dplyr::select(quarter_name_full, categories, variables_simple), 
            by = c("quarter_name_full", "categories", "variables_simple"))

####Save Outputs####
saveRDS(federal_courts_df_final, here("data", "federal_courts_df_final.rds"))
saveRDS(states_sf,               here("data", "states_sf.rds"))
saveRDS(judicial_district_sf,    here("data", "judicial_district_sf_simplified.rds"))
saveRDS(judicial_circuit_sf,     here("data", "judicial_circuit_sf_simplified.rds"))