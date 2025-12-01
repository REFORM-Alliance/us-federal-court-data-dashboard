rm(list = ls())
options(shiny.autoload.r = FALSE)


####Read in Libraries####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(DT)
library(leaflet)
library(sf)
library(tigris)
library(DBI)
library(dbplyr)
library(RPostgres)
library(lubridate)
library(scales)
library(bslib)
library(RColorBrewer)
library(plotly)
library(matrixStats)


####Connect to Database####
##Database Connection
con <- dbConnect(
  RPostgres::Postgres(),
  host = "reform-cjis-rds-cluster.cluster-cl8mgigamxxo.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "postgres",
  password = "Reform12345!"
)

on.exit(dbDisconnect(con), add = TRUE)


####Read in Data####
##Federal Courts Data
federal_courts_df <- 
  con %>% 
  tbl(in_schema("us_courts", "us_federal_courts_data")) %>% 
  collect() %>% 
  filter(!(table_num == "E-1" & variables == "persons_under_supervision" & table_name_date != date)) %>% 
  mutate(date_full = 
           ifelse(is.na(date) == TRUE, as.character(table_name_date), as.character(date)) %>% 
           as.Date(),
         date_month = 
           date_full %>% 
           month() %>% 
           as.numeric(), 
         year = 
           date_full %>% 
           year() %>% 
           as.numeric(),
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
  separate_wider_delim(state_or_district, names = c("state_abb", "district_abb"), delim = ",", cols_remove = FALSE, too_few = "align_start") %>% 
  left_join(data.frame(state_name = state.name, 
                       state_abb = state.abb) %>% 
              bind_rows(data.frame(state_name = c("District of Columbia", 
                                                  "Puerto Rico", 
                                                  "Guam", 
                                                  "Virgin Islands", 
                                                  "Northern Mariana Islands"), 
                                   state_abb = c("DC", "PR", "GU", "VI", "MP"))), 
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
           ifelse(is.na(district_name) == TRUE, state_name, paste(state_name, district_name, sep = " ")),
         full_district_name = 
           full_district_name %>% 
           str_replace_all("^North ", "Northern ") %>% 
           str_replace_all("^South ", "Southern ") %>% 
           str_replace_all("^East ", "Eastern ") %>% 
           str_replace_all("^West ", "Western ") %>%
           str_replace_all("South$", "Southern") %>% 
           str_replace_all("West$", "Western") %>% 
           str_replace_all("East$", "Eastern") %>% 
           str_replace_all("North$", "Northern") %>% 
           str_replace_all("Western Virginia", "West Virginia") %>% 
           str_replace_all("Southern Dakota", "South Dakota") %>% 
           str_replace_all("Northern Dakota", "North Dakota") %>% 
           str_replace_all("Southern Carolina", "South Carolina") %>% 
           str_replace_all("Northern Carolina", "North Carolina") %>% 
           str_replace_all("North Mariana Islands", "Northern Mariana Islands")) %>% 
  left_join("data-raw" %>% 
              here("other") %>% 
              here("districts_with_states.csv") %>% 
              fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
              clean_names() %>% 
              mutate(district_name_clean = 
                       district_name %>% 
                       str_replace_all("Penn[.]", "Pennsylvania") %>% 
                       str_replace_all("^N ", "North ") %>% 
                       str_replace_all("^W ", "West ") %>% 
                       str_replace_all("Cent$", "Central") %>% 
                       str_replace_all("Mid$", "Middle") %>% 
                       str_replace_all("Dist of Columbia", "District of Columbia") %>% 
                       str_replace_all("^North ", "Northern ") %>% 
                       str_replace_all("^South ", "Southern ") %>% 
                       str_replace_all("^East ", "Eastern ") %>% 
                       str_replace_all("^West ", "Western ") %>% 
                       str_replace_all("South$", "Southern") %>% 
                       str_replace_all("West$", "Western") %>% 
                       str_replace_all("East$", "Eastern") %>% 
                       str_replace_all("North$", "Northern") %>% 
                       str_replace_all("Western Virginia", "West Virginia") %>% 
                       str_replace_all("Southern Dakota", "South Dakota") %>% 
                       str_replace_all("Northern Dakota", "North Dakota") %>% 
                       str_replace_all("Southern Carolina", "South Carolina") %>% 
                       str_replace_all("Northern Carolina", "North Carolina") %>% 
                       str_replace_all("North Mariana Islands", "Northern Mariana Islands")) %>% 
              dplyr::select(district_name_clean, district_code) %>% 
              distinct(),
            by = c("full_district_name" = "district_name_clean")) %>% 
  dplyr::select(-c(state_abb, district_abb, district_name)) %>% 
  mutate(quarter_name_full = 
           paste0(year, ", Q", date_quarter)) %>% 
  rename("state_district" = "full_district_name") %>% 
  mutate(circuit_and_district = 
           ifelse(state_or_district == "DC", "DC", circuit_and_district)) %>% 
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
  read_rds()

##Judicial Circuits Geometries
judicial_circuit_sf <- 
  "data" %>% 
  here("judicial_circuit_sf.rds") %>% 
  read_rds()

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
                  dplyr::summarize(estimate =
                                     estimate %>%
                                     sum(na.rm = TRUE)) %>%
                  ungroup() %>%
                  mutate(state_or_district = circuit))
  } %>%
  {
    df <- .
    
    df %>%
      bind_rows(df %>% 
                  filter(is.na(state_district) == FALSE) %>% 
                  group_by(year) %>% 
                  dplyr::summarize(estimate = 
                                     estimate %>% 
                                     sum(na.rm = TRUE)) %>% 
                  ungroup() %>% 
                  mutate(state_or_district = "TOTAL"))
  } %>% 
  filter(is.na(year) == FALSE)

##Add Populations
federal_courts_df_new <- 
  federal_courts_df %>% 
  mutate(state_or_district = 
           state_or_district %>% 
           replace_na("99999")) %>% 
  left_join(judicial_district_pops %>% 
              dplyr::select(year, estimate, state_or_district) %>% 
              distinct() %>% 
              rename("population" = "estimate") %>% 
              mutate(state_or_district = 
                       state_or_district %>% 
                       replace_na("99999")), 
            by = c("year", "state_or_district")) %>% 
  arrange(state_or_district, year) %>% 
  group_by(state_or_district) %>% 
  tidyr::fill(population, .direction = "down") %>% 
  ungroup() %>% 
  mutate(state_or_district = 
           state_or_district %>% 
           na_if("99999"), 
         values_per_capita = 
           10000 * (values/population))


####App####
##Define Theme
my_theme <- bs_theme(
  version = 4,
  bg = "#FDF6E3",          
  fg = "#2C2C2C",     
  primary = "#009E73", 
  # secondary = "#DAA520",
  secondary = "#FFBF00",
  warning = "#FFBF00",
  # warning = "#FF6F61",     
  info = "#4A90E2",        
  success = "#2CA02C",     
  danger = "#D55E00",      
  base_font = font_google("Roboto")
)

##Define Quarters
quarters <- 
  federal_courts_df_new %>% 
  dplyr::select(year, date_quarter) %>% 
  distinct() %>% 
  arrange(year, date_quarter) %>% 
  mutate(keep = 1, 
         keep = 
           keep %>% 
           cumsum())

quarters_full <- 
  expand_grid(year = unique(quarters$year), date_quarter = 1:4) %>% 
  mutate(quarter_name_full = 
           paste0(year, ", Q", date_quarter)) %>% 
  left_join(quarters, by = c("year", "date_quarter")) %>% 
  tidyr::fill(keep, .direction = "down") %>% 
  filter(is.na(keep) == FALSE) %>% 
  filter(!(keep == max(keep, na.rm = TRUE) & date_quarter != min(date_quarter, na.rm = TRUE))) %>% 
  pull(quarter_name_full)
  
selected_quarters <- 
  ifelse(str_detect(quarters_full, "Q1$") == TRUE, quarters_full, "") %>% 
  str_replace_all(", Q[:digit:]", "")


####App####
##UI
# ui <- navbarPage("US Federal Court Supervision Data Dashboard",
#                  theme = my_theme,
#                  tags$head(
#                    tags$style(HTML("
#                               /* Hide most year range grid labels */
#                               #year_range .irs-grid-text,
#                               #year_range_state .irs-grid-text,
#                               #year_range_table .irs-grid-text {
#                                 display: none !important;
#                               }
#                               #year_range .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
#                               #year_range_state .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
#                               #year_range_table .irs-grid-pol:nth-child(16n+1) + .irs-grid-text {
#                                 display: block !important;
#                                 font-size: 10px !important;
#                               }
#                               
#                               /* Fixed navbar */
#                               .navbar {
#                                 position: fixed !important;
#                                 top: 0 !important;
#                                 left: 0 !important;
#                                 right: 0 !important;
#                                 z-index: 1030 !important;
#                                 margin-bottom: 0 !important;
#                               }
#                               
#                               /* Add padding to body to account for fixed navbar */
#                               body {
#                                 padding-top: 60px !important;
#                               }
#                               
#                               /* Fixed sidebar for both dashboards */
#                               .col-sm-2 {
#                                 position: fixed !important;
#                                 top: 60px !important;
#                                 left: 0 !important;
#                                 width: 16.66667% !important;
#                                 height: calc(100vh - 60px) !important;
#                                 overflow-y: auto !important;
#                                 z-index: 1000 !important;
#                                 background-color: #FDF6E3 !important;
#                                 padding-right: 15px !important;
#                               }
#                               
#                               /* Main panel offset for both dashboards */
#                               .col-sm-10 {
#                                 margin-left: 16.66667% !important;
#                                 width: 83.33333% !important;
#                                 padding-top: 20px !important;
#                                 padding-left: 30px !important;
#                               }
#                               
#                               /* Ensure box content doesn't get hidden */
#                               .box {
#                                 width: 100% !important;
#                                 overflow: visible !important;
#                               }
#                               
#                               /* Ensure plotly charts have proper spacing */
#                               .plotly {
#                                 margin-left: 0 !important;
#                               }
#                             "))
#                  ),
#                  tabPanel("Main Dashboard",
#                           # tags$head(
#                           #   tags$style(HTML("
#                           #         #year_range .irs-grid-text {
#                           #           display: none !important;
#                           #         }
#                           #         #year_range .irs-grid-pol:nth-child(16n+1) + .irs-grid-text {
#                           #           display: block !important;
#                           #           font-size: 10px !important;
#                           #         }
#                           # 
#                           #         .col-sm-2 {
#                           #           position: fixed !important;
#                           #           top: 50px;
#                           #           left: 0;
#                           #           height: 100vh;
#                           #           overflow-y: auto;
#                           #           z-index: 1000;
#                           #         }
#                           # 
#                           #         # .col-sm-10 {
#                           #         #   margin-left: 16.66667%;
#                           #         # }
#                           #       "))
#                           # ),
#                           # tags$head(
#                           #   tags$style(HTML("
#                           #     /* Hide most year range grid labels */
#                           #     #year_range .irs-grid-text,
#                           #     #year_range_state .irs-grid-text,
#                           #     #year_range_table .irs-grid-text {
#                           #       display: none !important;
#                           #     }
#                           #     #year_range .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
#                           #     #year_range_state .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
#                           #     #year_range_table .irs-grid-pol:nth-child(16n+1) + .irs-grid-text {
#                           #       display: block !important;
#                           #       font-size: 10px !important;
#                           #     }
#                           #     
#                           #     /* Fixed navbar */
#                           #     .navbar {
#                           #       position: fixed !important;
#                           #       top: 0 !important;
#                           #       left: 0 !important;
#                           #       right: 0 !important;
#                           #       z-index: 1030 !important;
#                           #       margin-bottom: 0 !important;
#                           #     }
#                           #     
#                           #     /* Add padding to body to account for fixed navbar */
#                           #     body {
#                           #       padding-top: 60px !important;
#                           #     }
#                           #     
#                           #     /* Fixed sidebar for both dashboards */
#                           #     .col-sm-2 {
#                           #       position: fixed !important;
#                           #       top: 60px !important;
#                           #       left: 0 !important;
#                           #       width: 16.66667% !important;
#                           #       height: calc(100vh - 60px) !important;
#                           #       overflow-y: auto !important;
#                           #       z-index: 1000 !important;
#                           #       background-color: #FDF6E3 !important;
#                           #       padding-right: 15px !important;
#                           #     }
#                           #     
#                           #     /* Main panel offset for both dashboards */
#                           #     .col-sm-10 {
#                           #       margin-left: 16.66667% !important;
#                           #       width: 83.33333% !important;
#                           #       padding-top: 20px !important;
#                           #     }
#                           #   "))
#                           # ),
#                           sidebarLayout(
#                             sidebarPanel(
#                               width = 2,
#                               tabsetPanel(
#                                 tabPanel(
#                                   "Map and Barplot Filters",
#                                   selectInput("outcome_type", "Outcome Metric Type",
#                                               choices = c("Population", "Admissions", "Exits"),
#                                               selected = "Population"),
#                                   selectInput("rate_or_count", "Rate or Count",
#                                               choices = c("Per Capita", "Total Count"),
#                                               selected = "Per Capita"),
#                                   selectInput("supervision_type", "Supervision Type",
#                                               choices = c("Total Supervision", "Supervised Release", "Probation", "Parole", "BOP Custody"),
#                                               selected = "Total Supervision"),
#                                   selectInput("agg_level", "Aggregation Type",
#                                               choices = c("Mean" = "mean",
#                                                           "Median" = "median",
#                                                           "Minimum" = "min",
#                                                           "Maximum" = "max"),
#                                               selected = "mean"),
#                                   selectInput("geo_level", "Geography Level", 
#                                               choices = c(
#                                                 "State" = "state_name", 
#                                                 "Judicial District" = "state_district", 
#                                                 "Judicial Circuit" = "circuit"
#                                               )),
#                                   sliderTextInput(
#                                     inputId = "year_range",
#                                     label = "Year Range",
#                                     choices = quarters_full,
#                                     selected = c(first(quarters_full), last(quarters_full)),
#                                     grid = FALSE,
#                                     dragRange = TRUE,
#                                     force_edges = TRUE
#                                   ),
#                                   verbatimTextOutput("selected_quarters")
#                                 ),
#                                 tabPanel(
#                                   "Table Filters",
#                                   selectInput("supervision_type_table", "Supervision Type",
#                                               choices = c("Total Supervision", "Supervised Release", "Probation", "Parole", "BOP Custody"),
#                                               selected = "Total Supervision", 
#                                               multiple = TRUE),
#                                   selectInput("rate_or_count_table", "Rate or Count",
#                                               choices = c("Per Capita", "Total Count"),
#                                               selected = "Per Capita"),
#                                   selectInput("geo_level_table", "Geography Level", 
#                                               choices = c(
#                                                 "State" = "state_name", 
#                                                 "Judicial District" = "state_district", 
#                                                 "Judicial Circuit" = "circuit"
#                                               )),
#                                   uiOutput("geo_filter_ui"), 
#                                   sliderTextInput(
#                                     inputId = "year_range_table",
#                                     label = "Year Range",
#                                     choices = quarters_full,
#                                     selected = c(first(quarters_full), last(quarters_full)),
#                                     grid = FALSE,
#                                     dragRange = TRUE,
#                                     force_edges = TRUE
#                                   )
#                                 )
#                               )
#                             ),
#                             mainPanel(
#                               width = 10,
#                               fluidRow(
#                                 box(width = 7, leafletOutput("map", height = 500)),
#                                 box(width = 5, 
#                                     align = "left",
#                                     div(style = "width:95%; padding:0; margin:0; text-align:left;",
#                                         plotlyOutput("barplot", height = 500, width = "100%")
#                                     )
#                                 )
#                               ),
#                               fluidRow(
#                                 box(width = 12, DTOutput("table"))
#                               )
#                             )
#                           )
#                  ),
#                  tabPanel(
#                    "State Dashboard",
#                    sidebarLayout(
#                      sidebarPanel(
#                        width = 2,
#                          selectInput("state_selection", "State", 
#                                      choices = sort(na.omit(unique(federal_courts_df_new$state_name)))),
#                          sliderTextInput(
#                            inputId = "year_range_state",
#                            label = "Year Range",
#                            choices = quarters_full,
#                            selected = c(first(quarters_full), last(quarters_full)),
#                            grid = FALSE,
#                            dragRange = TRUE,
#                            force_edges = TRUE
#                          )
#                        ),
#                      mainPanel(
#                        fluidRow(
#                          box(width = 12, 
#                              div(style = "width:100%; padding:0; margin:0; text-align:right;",
#                                  plotlyOutput("state_viz_stacked_barplot", height = 600, width = "100%")))
#                        ),
#                        fluidRow(
#                          box(width = 12, 
#                              div(style = "width:100%; padding:0; margin:0; text-align:right;",
#                                  plotlyOutput("state_viz_vs_national", height = 800, width = "100%")))
#                        )
#                      )
#                    )
#                  )
# )

####App####
##UI
ui <- navbarPage("US Federal Court Supervision Data Dashboard",
                 theme = my_theme,
                 tags$head(
                   tags$style(HTML("
      /* --- GLOBAL STYLES --- */
      
      /* Hide most year range grid labels */
      #year_range .irs-grid-text,
      #year_range_state .irs-grid-text,
      #year_range_table .irs-grid-text {
        display: none !important;
      }
      #year_range .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
      #year_range_state .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
      #year_range_table .irs-grid-pol:nth-child(16n+1) + .irs-grid-text {
        display: block !important;
        font-size: 10px !important;
      }
      
      /* Fixed navbar */
      .navbar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 1030 !important;
        margin-bottom: 0 !important;
      }
      
      /* Add padding to body to account for fixed navbar */
      body {
        padding-top: 60px !important;
      }
      
      /* --- FIXED SIDEBAR & OFFSET LOGIC FOR BOTH TABS --- */
      
      /* Apply fixed positioning to the Sidebar (.col-sm-2) 
         inside BOTH #dashboard_main AND #dashboard_state 
      */
      #dashboard_main .col-sm-2,
      #dashboard_state .col-sm-2 {
        position: fixed !important;
        top: 60px !important;
        left: 0 !important;
        width: 16.66667% !important;
        height: calc(100vh - 60px) !important; /* Full height minus navbar */
        overflow-y: auto !important; /* Allow sidebar to scroll internally if filters are long */
        z-index: 1000 !important;
        background-color: #FDF6E3 !important;
        padding-right: 15px !important;
        padding-left: 15px !important;
      }
      
      /* Apply margin offset to the Main Panel (.col-sm-10)
         inside BOTH #dashboard_main AND #dashboard_state 
      */
      #dashboard_main .col-sm-10,
      #dashboard_state .col-sm-10 {
        margin-left: 16.66667% !important; /* Push content to the right */
        width: 83.33333% !important;
        padding-top: 20px !important;
        padding-left: 30px !important;
      }
      
      /* --- CHART & BOX HELPERS --- */
      
      /* Ensure box content doesn't get hidden */
      .box {
        width: 100% !important;
        overflow: visible !important;
      }
      
      /* Ensure plotly charts have proper spacing */
      .plotly {
        margin-left: 0 !important;
      }
    "))
                 ),
                 
                 # --- TAB 1: MAIN DASHBOARD ---
                 tabPanel("Main Dashboard",
                          div(id = "dashboard_main", # <--- Wrapper ID 1
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  tabsetPanel(
                                    tabPanel(
                                      "Map and Barplot Filters",
                                      selectInput("outcome_type", "Outcome Metric Type",
                                                  choices = c("Population", "Admissions", "Exits"),
                                                  selected = "Population"),
                                      selectInput("rate_or_count", "Rate or Count",
                                                  choices = c("Per Capita", "Total Count"),
                                                  selected = "Per Capita"),
                                      selectInput("supervision_type", "Supervision Type",
                                                  choices = c("Total Supervision", "Supervised Release", "Probation", "Parole", "BOP Custody"),
                                                  selected = "Total Supervision"),
                                      selectInput("agg_level", "Aggregation Type",
                                                  choices = c("Mean" = "mean",
                                                              "Median" = "median",
                                                              "Minimum" = "min",
                                                              "Maximum" = "max"),
                                                  selected = "mean"),
                                      selectInput("geo_level", "Geography Level", 
                                                  choices = c(
                                                    "State" = "state_name", 
                                                    "Judicial District" = "state_district", 
                                                    "Judicial Circuit" = "circuit"
                                                  )),
                                      sliderTextInput(
                                        inputId = "year_range",
                                        label = "Year Range",
                                        choices = quarters_full,
                                        selected = c(first(quarters_full), last(quarters_full)),
                                        grid = FALSE,
                                        dragRange = TRUE,
                                        force_edges = TRUE
                                      ),
                                      verbatimTextOutput("selected_quarters")
                                    ),
                                    tabPanel(
                                      "Table Filters",
                                      selectInput("supervision_type_table", "Supervision Type",
                                                  choices = c("Total Supervision", "Supervised Release", "Probation", "Parole", "BOP Custody"),
                                                  selected = "Total Supervision", 
                                                  multiple = TRUE),
                                      selectInput("rate_or_count_table", "Rate or Count",
                                                  choices = c("Per Capita", "Total Count"),
                                                  selected = "Per Capita"),
                                      selectInput("geo_level_table", "Geography Level", 
                                                  choices = c(
                                                    "State" = "state_name", 
                                                    "Judicial District" = "state_district", 
                                                    "Judicial Circuit" = "circuit"
                                                  )),
                                      uiOutput("geo_filter_ui"), 
                                      sliderTextInput(
                                        inputId = "year_range_table",
                                        label = "Year Range",
                                        choices = quarters_full,
                                        selected = c(first(quarters_full), last(quarters_full)),
                                        grid = FALSE,
                                        dragRange = TRUE,
                                        force_edges = TRUE
                                      )
                                    )
                                  )
                                ),
                                mainPanel(
                                  width = 10,
                                  fluidRow(
                                    box(width = 7, leafletOutput("map", height = 500)),
                                    box(width = 5, 
                                        align = "left",
                                        div(style = "width:95%; padding:0; margin:0; text-align:left;",
                                            plotlyOutput("barplot", height = 500, width = "100%")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width = 12, DTOutput("table"))
                                  )
                                )
                              )
                          )
                 ),
                 
                 # --- TAB 2: STATE DASHBOARD ---
                 tabPanel("State Dashboard",
                          div(id = "dashboard_state", # <--- Wrapper ID 2
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  selectInput("state_selection", "State", 
                                              choices = sort(na.omit(unique(federal_courts_df_new$state_name)))),
                                  sliderTextInput(
                                    inputId = "year_range_state",
                                    label = "Year Range",
                                    choices = quarters_full,
                                    selected = c(first(quarters_full), last(quarters_full)),
                                    grid = FALSE,
                                    dragRange = TRUE,
                                    force_edges = TRUE
                                  )
                                ),
                                mainPanel(
                                  width = 10,
                                  fluidRow(
                                    box(width = 12, 
                                        div(style = "width:100%; padding:0; margin:0; text-align:right;",
                                            plotlyOutput("state_viz_stacked_barplot", height = 600, width = "100%")))
                                  ),
                                  fluidRow(
                                    box(width = 12, 
                                        div(style = "width:100%; padding:0; margin:0; text-align:right;",
                                            plotlyOutput("state_viz_vs_national", height = 800, width = "100%")))
                                  )
                                )
                              )
                          )
                 )
)

##Server
server <- function(input, output, session){
  
  observeEvent(c(input$outcome_type, input$rate_or_count), {
    if(input$rate_or_count == "Per Capita"){
      updateSelectInput(session, "agg_level",
                        choices = c("Mean" = "mean",
                                    "Median" = "median",
                                    "Minimum" = "min",
                                    "Maximum" = "max"),
                        selected = "mean")
    } else if(input$outcome_type %in% c("Admissions", "Exits")){
      updateSelectInput(session, "agg_level",
                        choices = c("Mean" = "mean",
                                    "Median" = "median",
                                    "Minimum" = "min",
                                    "Maximum" = "max",
                                    "Sum" = "sum"),
                        selected = "sum")
    } else {
      updateSelectInput(session, "agg_level",
                        choices = c("Mean" = "mean",
                                    "Median" = "median",
                                    "Minimum" = "min",
                                    "Maximum" = "max"),
                        selected = "mean")
    }
  })
  
  output$geo_filter_ui <- renderUI({
    req(input$geo_level_table)  
    geo_level <- input$geo_level_table  
    
    choices <- sort(na.omit(unique(federal_courts_df_new[[geo_level]])))
    geo_name = 
      geo_level %>% 
      str_replace_all("state_name", "State") %>% 
      str_replace_all("state_district", "Judicial District") %>% 
      str_replace_all("circuit", "Judicial Circuit")
    
    selectizeInput("geo_table", geo_name,
                   choices = choices, 
                   multiple = TRUE)
  }) 
  
  filtered_data <- reactive({
    req(input$year_range)
    
    start_idx <- which(quarters_full == input$year_range[1])
    end_idx <- which(quarters_full == input$year_range[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    federal_courts_df_new %>%
      filter(
        quarter_name_full %in% selected_quarters,
        variables == selected_variable()
      )
  })
  
  filtered_data_full <- reactive({
    req(input$year_range_table)
    
    start_idx <- which(quarters_full == input$year_range_table[1])
    end_idx <- which(quarters_full == input$year_range_table[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    if(input$rate_or_count_table == "Per Capita"){
      federal_courts_df_new %>%
        filter(quarter_name_full %in% selected_quarters) %>% 
        dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population, variables, table_num) %>% 
        group_by(quarter_name_full, variables, .data[[input$geo_level_table]], table_num) %>% 
        dplyr::summarize(across(c(values, population),
                                ~.x %>% 
                                  sum(na.rm = TRUE))) %>% 
        ungroup() %>% 
        mutate(values = 
                 10000 * (values/population),
               outcome_metric_type = 
                 case_when(str_detect(variables, "^received_for_post_conviction_supervision") == TRUE & 
                             table_num == "E-1" ~ "Admissions", 
                           str_detect(variables, "^removed_from_post_conviction_supervision") == TRUE & 
                             table_num == "E-1" ~ "Exits", 
                           variables == "persons_under_supervision" & 
                             table_num == "E-2" ~ "Total Population",
                           str_detect(variables, "^from_") == TRUE & 
                             table_num == "E-2" ~ "Total Population", 
                           TRUE ~ NA), 
               supervision_type = 
                 case_when(str_detect(variables, "bop_custody$") ~ "BOP Custody", 
                           str_detect(variables, "supervised_release$") ~ "Supervised Release", 
                           str_detect(variables, "parole$") ~ "Parole", 
                           str_detect(variables, "probation$") ~ "Probation", 
                           str_detect(variables, "total_removed$") ~ "Total Supervision", 
                           str_detect(variables, "total_received$") ~ "Total Supervision", 
                           variables == "persons_under_supervision" ~ "Total Supervision", 
                           TRUE ~ NA),
               outcome_metric_type = 
                 ifelse(is.na(outcome_metric_type) == TRUE, NA, paste0(outcome_metric_type, " per 10,000"))) %>% 
        filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE) %>% 
        dplyr::select(quarter_name_full, .data[[input$geo_level_table]], supervision_type, outcome_metric_type, values) %>% 
        pivot_wider(names_from = "outcome_metric_type", 
                    values_from = "values") %>% 
        rename_with(~.x %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title())
    }else{
      federal_courts_df_new %>%
        filter(quarter_name_full %in% selected_quarters) %>% 
        dplyr::select(circuit, state_district, state_name, quarter_name_full, values, variables, table_num) %>% 
        group_by(quarter_name_full, variables, .data[[input$geo_level_table]], table_num) %>% 
        dplyr::summarize(values = 
                           values %>% 
                           sum(na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(outcome_metric_type = 
                 case_when(str_detect(variables, "^received_for_post_conviction_supervision") == TRUE & 
                             table_num == "E-1" ~ "Admissions", 
                           str_detect(variables, "^removed_from_post_conviction_supervision") == TRUE & 
                             table_num == "E-1" ~ "Exits", 
                           variables == "persons_under_supervision" & 
                             table_num == "E-2" ~ "Total Population",
                           str_detect(variables, "^from_") == TRUE & 
                             table_num == "E-2" ~ "Total Population", 
                           TRUE ~ NA), 
               supervision_type = 
                 case_when(str_detect(variables, "bop_custody$") ~ "BOP Custody", 
                           str_detect(variables, "supervised_release$") ~ "Supervised Release", 
                           str_detect(variables, "parole$") ~ "Parole", 
                           str_detect(variables, "probation$") ~ "Probation", 
                           str_detect(variables, "total_removed$") ~ "Total Supervision", 
                           str_detect(variables, "total_received$") ~ "Total Supervision", 
                           variables == "persons_under_supervision" ~ "Total Supervision", 
                           TRUE ~ NA)) %>% 
        filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE) %>% 
        dplyr::select(quarter_name_full, .data[[input$geo_level_table]], supervision_type, outcome_metric_type, values) %>% 
        pivot_wider(names_from = "outcome_metric_type", 
                    values_from = "values") %>% 
        rename_with(~.x %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title())
    }
  })
  
  agg_data <- reactive({
    req(input$agg_level)
    
    agg_fun <- switch(input$agg_level,
                      "mean" = mean,
                      "median" = median,
                      "min" = min,
                      "max" = max, 
                      "sum" = sum)
    
    if(input$rate_or_count == "Per Capita"){
      if(input$agg_level == "mean"){
        filtered_data() %>%
          dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population) %>% 
          group_by(.data[[input$geo_level]]) %>% 
          dplyr::summarize(across(c(values, population), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(value = 
                   10000 * (values/population)) %>%
          ungroup()
      }else if(input$agg_level == "median"){
        filtered_data() %>%
          dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population) %>% 
          group_by(.data[[input$geo_level]], quarter_name_full) %>%
          dplyr::summarize(across(c(values, population), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(per_capita = 10000 * (values/population)) %>% 
          group_by(.data[[input$geo_level]]) %>%
          dplyr::summarize(value = 
                             per_capita %>% 
                             weightedMedian(w = population, 
                                            na.rm = TRUE)) %>% 
          ungroup()
      }else{
        filtered_data() %>%
          dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population) %>% 
          group_by(.data[[input$geo_level]], quarter_name_full) %>%
          dplyr::summarize(across(c(values, population), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(per_capita = 
                   10000 * (values/population)) %>% 
          group_by(.data[[input$geo_level]]) %>%
          dplyr::summarize(value =
                             per_capita %>%
                             agg_fun(na.rm = TRUE)) %>%
          ungroup()
      }
    }else{
      filtered_data() %>%
        dplyr::select(circuit, state_district, state_name, quarter_name_full, values) %>%
        group_by(.data[[input$geo_level]], quarter_name_full) %>%
        dplyr::summarize(
          value_agg =
            values %>%
            sum(na.rm = TRUE)
        ) %>%
        ungroup() %>%
        group_by(.data[[input$geo_level]]) %>%
        dplyr::summarize(value =
                             value_agg %>%
                             agg_fun(na.rm = TRUE)) %>%
        ungroup()
    }
  })
  
  selected_variable <- reactive({
    type <- input$outcome_type
    sup <- input$supervision_type
    
    if(type == "Population"){
      var <- switch(sup,
                    "Total Supervision" = "persons_under_supervision",
                    "Supervised Release" = "from_institutions_term_of_supervised_release",
                    "Probation" = "from_courts_probation",
                    "Parole" = "from_institutions_parole",
                    "BOP Custody" = "from_institutions_bop_custody")
    }else if(type == "Admissions"){
      var <- switch(sup,
                    "Total Supervision" = "received_for_post_conviction_supervision_total_received",
                    "Supervised Release" = "received_for_post_conviction_supervision_term_of_supervised_release",
                    "Probation" = "received_for_post_conviction_supervision_probation",
                    "Parole" = "received_for_post_conviction_supervision_parole",
                    "BOP Custody" = "received_for_post_conviction_supervision_bop_custody")
    }else if(type == "Exits"){
      var <- switch(sup,
                    "Total Supervision" = "removed_from_post_conviction_supervision_total_removed",
                    "Supervised Release" = "removed_from_post_conviction_supervision_term_of_supervised_release",
                    "Probation" = "removed_from_post_conviction_supervision_probation",
                    "Parole" = "removed_from_post_conviction_supervision_parole",
                    "BOP Custody" = "removed_from_post_conviction_supervision_bop_custody")
    }
    return(var)
  })
  
  output$map <- renderLeaflet({
    df_map <- agg_data()
    
    if(input$rate_or_count == "Per Capita"){
      legend_title <- 
        paste(str_to_title(input$agg_level),
              input$supervision_type,
              input$outcome_type, 
              "per 10,000") %>% 
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      if(input$geo_level == "state_name"){
        map_data <- 
          states_sf %>%
          left_join(df_map, by = "state_name")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(state_name, ": ", round(value, 2)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }else if(input$geo_level == "state_district"){
        map_data <- 
          judicial_district_sf %>%
          left_join(df_map, by = "state_district")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(state_district, ": ", round(value, 2)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }else if(input$geo_level == "circuit"){
        map_data <- 
          judicial_circuit_sf %>%
          left_join(df_map, by = "circuit")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(circuit, ": ", round(value, 2)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }
    }else{
      legend_title <- 
        paste(str_to_title(input$agg_level),
              input$supervision_type,
              input$outcome_type) %>% 
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      if(input$geo_level == "state_name"){
        map_data <- 
          states_sf %>%
          left_join(df_map, by = "state_name")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(state_name, ": ", scales::comma_format()(value)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }else if(input$geo_level == "state_district"){
        map_data <- 
          judicial_district_sf %>%
          left_join(df_map, by = "state_district")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(state_district, ": ", scales::comma_format()(value)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }else if(input$geo_level == "circuit"){
        map_data <- 
          judicial_circuit_sf %>%
          left_join(df_map, by = "circuit")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
          mutate(value = 
                   value %>% 
                   replace_na(0) %>% 
                   as.numeric()) %>% 
          leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(value),
            color = "white", weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(circuit, ": ", scales::comma_format()(value)),
            highlightOptions = highlightOptions(
              weight = 3,           
              color = "#E69F00",       
              fillOpacity = 0.9,    
              bringToFront = TRUE   
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = ~value,
                    title = legend_title,
                    opacity = 0.8) %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }
    }
  })
  
  output$barplot <- renderPlotly({
    
    if(input$rate_or_count == "Per Capita"){
      x_lab <-
        input$agg_level %>% 
        str_to_title() %>% 
        paste(input$supervision_type,
              input$outcome_type, 
              "per 10,000")
      
      df <- 
        agg_data() %>%
        filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
        mutate(across(c(any_of(input$geo_level)), ~replace_na(.x, "Missing/Not Applicable")),
               value = 
                 value %>% 
                 as.numeric())  
      
      ggplot_barplot_main <- 
        df %>%
        mutate(geo_reorder = reorder(.data[[input$geo_level]], value)) %>%
        ggplot(aes(
          x = value,
          y = geo_reorder,
          fill = value,
          text = paste0(.data[[input$geo_level]], ": ", round(value, 2))
        )) +
        geom_col() +  
        scale_fill_distiller(palette = "YlGn", 
                             direction = 1, 
                             name = NULL) +
        labs(
          x = 
            x_lab,
          y = 
            input$geo_level %>%
            str_replace_all("state_name", "State") %>%
            str_replace_all("state_district", "Judicial District") %>%
            str_replace_all("circuit", "Judicial Circuit")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 8, margin = margin(r = 0)),  
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10))
        )
    }else{
      x_lab <-
        input$agg_level %>% 
        str_to_title() %>% 
        paste(input$supervision_type,
              input$outcome_type)
      
      df <- 
        agg_data() %>%
        filter(is.na(.data[[input$geo_level]]) == FALSE) %>% 
        mutate(across(c(any_of(input$geo_level)), ~replace_na(.x, "Missing/Not Applicable")),
               value = 
                 value %>% 
                 as.numeric())  
      
      ggplot_barplot_main <- 
        df %>%
        mutate(geo_reorder = reorder(.data[[input$geo_level]], value)) %>%
        ggplot(aes(
          x = value,
          y = geo_reorder,
          fill = value,
          text = paste0(.data[[input$geo_level]], ": ", comma(value))
        )) +
        geom_col() +  
        scale_fill_distiller(palette = "YlGn", 
                             direction = 1, 
                             name = NULL,
                             labels = scales::comma) +
        scale_x_continuous(
          labels = comma_format()  
        ) +
        labs(
          x = 
            x_lab,
          y = 
            input$geo_level %>%
            str_replace_all("state_name", "State") %>%
            str_replace_all("state_district", "Judicial District") %>%
            str_replace_all("circuit", "Judicial Circuit")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 8, margin = margin(r = 0)),  
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10))
        )
    }
    
    barplot_main <- 
      ggplot_barplot_main %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(l = 50, r = 10, t = 50, b = 50),
        autosize = TRUE,
        showlegend = FALSE
      )
    
    barplot_main
  })
  
  output$table <- renderDT({
    if(input$rate_or_count_table == "Per Capita"){
      df <- 
        filtered_data_full() %>% 
        rename_with(~.x %>% 
                      str_replace_all("^Quarter Name Full$", "Year and Quarter")) %>% 
        mutate(across(where(is.numeric), 
                      ~.x %>% 
                        round(3)))
    }else{
      df <- 
        filtered_data_full() %>% 
        rename_with(~.x %>% 
                      str_replace_all("^Quarter Name Full$", "Year and Quarter")) %>% 
        mutate(across(where(is.numeric), 
                      ~.x %>% 
                        comma()))
    }
    
    datatable(
      df,
      extensions = c('Buttons'),
      class = 'compact stripe hover',
      filter = "top",  
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy','csv','excel'),
        paging = FALSE,             
        scrollY = "600px",          
        scrollX = TRUE,
        scrollCollapse = FALSE      
      ),
      style = "bootstrap",
      rownames = FALSE
    )
  })
  
  output$state_viz_vs_national <- renderPlotly({
    req(input$year_range_state)
    
    start_idx <- which(quarters_full == input$year_range_state[1])
    end_idx <- which(quarters_full == input$year_range_state[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    federal_courts_df_new %>%
      mutate(outcome_metric_type = 
               case_when(str_detect(variables, "^received_for_post_conviction_supervision") == TRUE & 
                           table_num == "E-1" ~ "Admissions", 
                         str_detect(variables, "^removed_from_post_conviction_supervision") == TRUE & 
                           table_num == "E-1" ~ "Exits", 
                         variables == "persons_under_supervision" & 
                           table_num == "E-2" ~ "Total Population",
                         str_detect(variables, "^from_") == TRUE & 
                           table_num == "E-2" ~ "Total Population", 
                         TRUE ~ NA), 
             supervision_type = 
               case_when(str_detect(variables, "bop_custody$") ~ "BOP Custody", 
                         str_detect(variables, "supervised_release$") ~ "Supervised Release", 
                         str_detect(variables, "parole$") ~ "Parole", 
                         str_detect(variables, "probation$") ~ "Probation", 
                         str_detect(variables, "total_removed$") ~ "Total Supervision", 
                         str_detect(variables, "total_received$") ~ "Total Supervision", 
                         variables == "persons_under_supervision" ~ "Total Supervision", 
                         TRUE ~ NA)) %>% 
      filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE) %>%
      dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population, outcome_metric_type, supervision_type) %>%
      group_by(state_name, quarter_name_full, outcome_metric_type, supervision_type) %>%
      dplyr::summarize(across(c(values, population), 
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>%
      mutate(values_per_capita = 
               10000 * (values/population)) %>% 
      separate_wider_delim(cols = quarter_name_full, delim = ", Q", names = c("year", "quarter"), cols_remove = FALSE) %>%
      mutate(quarter =
               quarter %>%
               as.numeric(),
             month =
               case_when(quarter == 1 ~ "03",
                         quarter == 2 ~ "06",
                         quarter == 3 ~ "09",
                         quarter == 4 ~ "12"),
             date =
               paste(year, month, "01", sep = "-") %>%
               as.Date(),
             date =
               date + months(1) - days(1)) %>%
      {
        base_data <- .
        bind_rows(
          base_data,
          base_data %>%  
            group_by(date, supervision_type, outcome_metric_type, quarter_name_full) %>%
            dplyr::summarize(across(c(values, population), 
                                    ~.x %>% 
                                      sum(na.rm = TRUE))) %>% 
            ungroup() %>% 
            mutate(values_per_capita = 
                     10000 * (values/population), 
                   state_name = "National Mean"),
          base_data %>%
            mutate(values_per_capita = 
                     10000 * (values/population)) %>% 
            group_by(date, supervision_type, outcome_metric_type, quarter_name_full) %>%
            dplyr::summarize(values_per_capita = 
                               values_per_capita %>% 
                               weightedMedian(w = population, 
                                              na.rm = TRUE)) %>% 
            ungroup() %>% 
            mutate(state_name = "National Median")
        )
      } %>% 
      filter(state_name %in% c(input$state_selection, "National Mean", "National Median"),
             quarter_name_full %in% selected_quarters) %>%
      mutate(supervision_type = 
               supervision_type %>% 
               factor(levels = rev(c("BOP Custody", "Supervised Release", 
                                     "Parole", "Probation", "Total Supervision"))),
             hover_text = paste0(
               "Supervision: ", supervision_type, "\n",
               "Outcome: ", outcome_metric_type, "\n",
               "State: ", state_name, "\n",
               # "Value: ", format(round(values, 0), big.mark = ","), "\n",
               "Value: ", round(values_per_capita, 2), "\n",
               "Date: ", format(date, "%b %d, %Y")
             )) %>%
      {
        ggplot(., aes(x = date,
                      y = values_per_capita,
                      color = state_name,
                      group = state_name,
                      text = hover_text)) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          facet_grid(supervision_type ~ outcome_metric_type, scales = "free_y") +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
          # scale_y_continuous(labels = scales::comma) +
          scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          labs(x = "Date", y = "Count", color = paste0(input$state_selection, " vs. National")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text = element_text(size = 12, face = "bold"),
                strip.text.y = element_text(angle = 0), 
                legend.position = "bottom",
                axis.title.x = element_text(margin = margin(t = 15)))
      } %>%
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      ),
      margin = list(l = 50, r = 180, t = 40, b = 55))
  })
  
  output$state_viz_stacked_barplot <- renderPlotly({
    req(input$year_range_state)
    
    start_idx <- which(quarters_full == input$year_range_state[1])
    end_idx <- which(quarters_full == input$year_range_state[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    federal_courts_df_new %>%
      mutate(outcome_metric_type = 
               case_when(str_detect(variables, "^received_for_post_conviction_supervision") == TRUE & 
                           table_num == "E-1" ~ "Admissions", 
                         str_detect(variables, "^removed_from_post_conviction_supervision") == TRUE & 
                           table_num == "E-1" ~ "Exits", 
                         variables == "persons_under_supervision" & 
                           table_num == "E-2" ~ "Total Population",
                         str_detect(variables, "^from_") == TRUE & 
                           table_num == "E-2" ~ "Total Population", 
                         TRUE ~ NA), 
             supervision_type = 
               case_when(str_detect(variables, "bop_custody$") ~ "BOP Custody", 
                         str_detect(variables, "supervised_release$") ~ "Supervised Release", 
                         str_detect(variables, "parole$") ~ "Parole", 
                         str_detect(variables, "probation$") ~ "Probation", 
                         str_detect(variables, "total_removed$") ~ "Total Supervision", 
                         str_detect(variables, "total_received$") ~ "Total Supervision", 
                         variables == "persons_under_supervision" ~ "Total Supervision", 
                         TRUE ~ NA)) %>% 
      filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE) %>%
      dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population, outcome_metric_type, supervision_type) %>%
      group_by(state_name, quarter_name_full, outcome_metric_type, supervision_type) %>%
      dplyr::summarize(across(c(values, population), 
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>% 
      separate_wider_delim(cols = quarter_name_full, delim = ", Q", names = c("year", "quarter"), cols_remove = FALSE) %>%
      mutate(quarter =
               quarter %>%
               as.numeric(),
             month =
               case_when(quarter == 1 ~ "03",
                         quarter == 2 ~ "06",
                         quarter == 3 ~ "09",
                         quarter == 4 ~ "12"),
             date =
               paste(year, month, "01", sep = "-") %>%
               as.Date(),
             date =
               date + months(1) - days(1)) %>% 
      filter(state_name == input$state_selection &
             quarter_name_full %in% selected_quarters) %>% 
      {
        base_data <- .
        base_data %>% 
          bind_rows(base_data %>% 
                      group_by(supervision_type, outcome_metric_type, state_name) %>% 
                      mutate(values = 
                               values %>% 
                               mean(na.rm = TRUE)) %>% 
                      ungroup())
      } %>% 
      mutate(supervision_type = 
               supervision_type %>% 
               factor(levels = rev(c("BOP Custody", "Supervised Release", 
                                     "Parole", "Probation", "Total Supervision"))),
             hover_text = paste0(
               "Supervision: ", supervision_type, "\n",
               "Outcome: ", outcome_metric_type, "\n",
               "State: ", state_name, "\n",
               "Mean Value: ", format(round(values, 0), big.mark = ","), "\n"
               # "Value: ", round(values_per_capita, 2), "\n",
               # "Year: ", year
             )) %>%
      dplyr::select(state_name, supervision_type, outcome_metric_type, values, hover_text) %>% 
      {
        ggplot(., aes(x = supervision_type,
                      y = values,
                      fill = supervision_type,
                      group = supervision_type,
                      text = hover_text)) +
          geom_col(aes(color = supervision_type, fill = supervision_type),
                   position = "stack", width = 0.99) +
          facet_wrap(~outcome_metric_type) +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_y_continuous(labels = scales::comma) +
          # scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
          # scale_x_continuous(breaks = seq(min(.$year), max(.$year), by = 5),
          #                    expand = c(0, 0)) +
          labs(x = "Year",  
               y = "Mean Count Over Time", 
               color = "Supervision Type", 
               fill = "Supervision Type") +
          theme_minimal() +
          theme(axis.text.x = element_blank(),  
                axis.ticks.x = element_blank(), 
                strip.text = element_text(size = 12, face = "bold"),
                strip.text.y = element_text(angle = 0), 
                legend.position = "bottom",
                axis.title.x = element_text(margin = margin(t = 15)))
      } %>%
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      ),
      margin = list(l = 50, r = 180, t = 40, b = 55))
  })
}
  

####Run App####
shinyApp(ui, server)

