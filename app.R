rm(list = ls())
options(shiny.autoload.r = FALSE)

####Read in Libraries####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(here)
library(janitor)
library(DT)
library(leaflet)
library(sf)
library(lubridate)
library(scales)
library(bslib)
library(plotly)
library(matrixStats)

####Load Pre-processed Data####
federal_courts_df_final <- readRDS(here("data", "federal_courts_df_final.rds"))
states_sf               <- readRDS(here("data", "states_sf.rds"))
judicial_district_sf    <- readRDS(here("data", "judicial_district_sf_simplified.rds"))
judicial_circuit_sf     <- readRDS(here("data", "judicial_circuit_sf_simplified.rds"))

####Define Theme####
my_theme <- bs_theme(
  version = 4,
  bg = "#FDF6E3",          
  fg = "#2C2C2C",     
  primary = "#009E73", 
  secondary = "#FFBF00",
  warning = "#FFBF00",
  info = "#4A90E2",        
  success = "#2CA02C",     
  danger = "#D55E00",      
  base_font = font_google("Roboto")
)

####Define Quarters####
quarters <- 
  federal_courts_df_final %>% 
  dplyr::select(year, date_quarter) %>% 
  distinct() %>% 
  arrange(year, date_quarter) %>% 
  mutate(keep = 1, keep = keep %>% cumsum())

quarters_full <- 
  expand_grid(year = unique(quarters$year), date_quarter = 1:4) %>% 
  mutate(quarter_name_full = paste0(year, ", Q", date_quarter)) %>% 
  left_join(quarters, by = c("year", "date_quarter")) %>% 
  tidyr::fill(keep, .direction = "down") %>% 
  filter(!is.na(keep)) %>% 
  mutate(max_keep = keep %>% max(na.rm = TRUE)) %>% 
  group_by(keep) %>% 
  mutate(filter_out = ifelse(keep == max_keep & date_quarter == max(date_quarter, na.rm = TRUE), 1, 0)) %>% 
  filter(!(filter_out == 1)) %>%  
  pull(quarter_name_full)

available_years <- sort(unique(federal_courts_df_final$year), decreasing = TRUE)

selected_quarters <- 
  ifelse(str_detect(quarters_full, "Q1$"), quarters_full, "") %>% 
  str_replace_all(", Q[:digit:]", "")


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
      #year_range_table .irs-grid-text,
      #year_range_national .irs-grid-text {
        display: none !important;
      }
      #year_range .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
      #year_range_state .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
      #year_range_table .irs-grid-pol:nth-child(16n+1) + .irs-grid-text,
      #year_range_national .irs-grid-pol:nth-child(16n+1) + .irs-grid-text {
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
      
      #dashboard_main .col-sm-2,
      #dashboard_state .col-sm-2,
      #dashboard_national .col-sm-2 {
        position: fixed !important;
        top: 60px !important;
        left: 0 !important;
        width: 16.66667% !important;
        height: calc(100vh - 60px) !important;
        overflow-y: auto !important;
        z-index: 1000 !important;
        background-color: #FDF6E3 !important;
        padding-right: 15px !important;
        padding-left: 15px !important;
      }
      
      #dashboard_main .col-sm-10,
      #dashboard_state .col-sm-10,
      #dashboard_national .col-sm-10 {
        margin-left: 16.66667% !important;
        width: 83.33333% !important;
        padding-top: 20px !important;
        padding-left: 30px !important;
      }
      
      /* --- CHART & BOX HELPERS --- */
      
      .box {
        width: 100% !important;
        overflow: visible !important;
      }
      
      .plotly {
        margin-left: 0 !important;
      }
      
      /* --- REPORTING NOTE STYLE --- */
      .reporting-note {
        font-size: 11px;
        color: #888;
        font-style: italic;
        margin-top: 6px;
        line-height: 1.4;
      }
      
      /* --- TOGGLE LABEL STYLE --- */
      .year-mode-toggle .radio label {
        font-size: 13px;
      }
    "))
                 ),
                 
                 tabPanel("Main Dashboard",
                          div(id = "dashboard_main", 
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  tabsetPanel(
                                    tabPanel(
                                      "Map and Barplot Filters",
                                      selectInput("outcome_type", "Outcome Metric Type",
                                                  choices = c("Population", "Admissions", "Exits"),
                                                  selected = "Population"),
                                      selectInput("rate_or_count", "Count or Per Capita",
                                                  choices = c("Per Capita", "Total Count"),
                                                  selected = "Total Count"),
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
                                      # CHANGE 1: Toggle between range and single year/quarter
                                      div(class = "year-mode-toggle",
                                          radioButtons("year_mode_map", "Date Selection Mode",
                                                       choices = c("Year/Quarter Range" = "range",
                                                                   "Single Year/Quarter" = "single"),
                                                       selected = "range",
                                                       inline = FALSE)
                                      ),
                                      uiOutput("year_selector_map"),
                                      # CHANGE 2: Reporting period note
                                      div(class = "reporting-note",
                                          "Note: All figures reflect the 12-month reporting period ending on the selected date(s)."),
                                      div(class = "reporting-note",
                                          style = "color: #cc7000;",
                                          "⚠ Note: Admissions and exits data from 2011 Q4–2012 Q2 had large discrepancies between district-level and national totals and have been excluded from visualizations.")
                                    ),
                                    tabPanel(
                                      "Table Filters",
                                      selectInput("supervision_type_table", "Supervision Type",
                                                  choices = c("Total Supervision", "Supervised Release", "Probation", "Parole", "BOP Custody"),
                                                  selected = "Total Supervision", 
                                                  multiple = TRUE),
                                      selectInput("rate_or_count_table", "Count or Per Capita",
                                                  choices = c("Per Capita", "Total Count"),
                                                  selected = "Total Count"),
                                      selectInput("geo_level_table", "Geography Level", 
                                                  choices = c(
                                                    "State" = "state_name", 
                                                    "Judicial District" = "state_district", 
                                                    "Judicial Circuit" = "circuit"
                                                  )),
                                      uiOutput("geo_filter_ui"),
                                      # CHANGE 1: Toggle between range and single year/quarter
                                      div(class = "year-mode-toggle",
                                          radioButtons("year_mode_table", "Date Selection Mode",
                                                       choices = c("Year/Quarter Range" = "range",
                                                                   "Single Year/Quarter" = "single"),
                                                       selected = "range",
                                                       inline = FALSE)
                                      ),
                                      uiOutput("year_selector_table"),
                                      # CHANGE 2: Reporting period note
                                      div(class = "reporting-note",
                                          "Note: All figures reflect the 12-month reporting period ending on the selected date(s)."),
                                      div(class = "reporting-note",
                                          style = "color: #cc7000;",
                                          "⚠ Note: Admissions and exits data from 2011 Q4–2012 Q2 had large discrepancies between district-level and national totals and have been excluded from visualizations.")
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
                 
                 tabPanel("State Dashboard",
                          div(id = "dashboard_state", 
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  selectInput("state_selection", "State", 
                                              choices = sort(na.omit(unique(federal_courts_df_final$state_name)))),
                                  selectInput("rate_or_count_state", "Count or Per Capita",
                                              choices = c("Per Capita", "Total Count"),
                                              selected = "Total Count"),
                                  sliderTextInput(
                                    inputId = "year_range_state",
                                    label = "Year Range",
                                    choices = quarters_full,
                                    selected = c(first(quarters_full), last(quarters_full)),
                                    grid = FALSE,
                                    dragRange = TRUE,
                                    force_edges = TRUE
                                  ),
                                  # CHANGE 2: Reporting period note
                                  div(class = "reporting-note",
                                      "Note: All figures reflect the 12-month reporting period ending on the selected date(s)."), 
                                  div(class = "reporting-note",
                                      style = "color: #cc7000;",
                                      "⚠ Note: Admissions and exits data from 2011 Q4–2012 Q2 had large discrepancies between district-level and national totals and have been excluded from visualizations.")
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
                 ),
                 
                 # CHANGE 3: New National Dashboard tab
                 tabPanel("National Dashboard",
                          div(id = "dashboard_national",
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  selectInput("rate_or_count_national", "Count or Per Capita",
                                              choices = c("Per Capita", "Total Count"),
                                              selected = "Total Count"),
                                  sliderTextInput(
                                    inputId = "year_range_national",
                                    label = "Year Range",
                                    choices = quarters_full,
                                    selected = c(first(quarters_full), last(quarters_full)),
                                    grid = FALSE,
                                    dragRange = TRUE,
                                    force_edges = TRUE
                                  ),
                                  # CHANGE 2: Reporting period note
                                  div(class = "reporting-note",
                                      "Note: All figures reflect the 12-month reporting period ending on the selected date(s)."), 
                                  div(class = "reporting-note",
                                      style = "color: #cc7000;",
                                      "⚠ Note: Admissions and exits data from 2011 Q4–2012 Q2 had large discrepancies between district-level and national totals and have been excluded from visualizations.")
                                ),
                                mainPanel(
                                  width = 10,
                                  fluidRow(
                                    box(width = 12,
                                        div(style = "width:100%; padding:0; margin:0; text-align:right;",
                                            plotlyOutput("national_viz_stacked_barplot", height = 600, width = "100%")))
                                  ),
                                  fluidRow(
                                    box(width = 12,
                                        div(style = "width:100%; padding:0; margin:0; text-align:right;",
                                            plotlyOutput("national_viz_trends", height = 800, width = "100%")))
                                  )
                                )
                              )
                          )
                 )
)

##Server
server <- function(input, output, session){
  
  # CHANGE 1: Render year selector UI for map/barplot filters (toggle-based)
  output$year_selector_map <- renderUI({
    if (input$year_mode_map == "single") {
      fluidRow(
        column(7,
               selectInput("single_year_map_val", "Year",
                           choices = available_years,
                           selected = max(available_years))
        ),
        column(5,
               selectInput("single_quarter_map_val", "Quarter",
                           choices = c("Q1", "Q2", "Q3", "Q4"),
                           selected = "Q1")
        )
      )
    } else {
      sliderTextInput(
        inputId = "year_range",
        label = "Year/Quarter Range",
        choices = quarters_full,
        selected = c(first(quarters_full), last(quarters_full)),
        grid = FALSE,
        dragRange = TRUE,
        force_edges = TRUE
      )
    }
  })
  
  # CHANGE 1: Render year selector UI for table filters (toggle-based)
  output$year_selector_table <- renderUI({
    if (input$year_mode_table == "single") {
      fluidRow(
        column(7,
               selectInput("single_year_table_val", "Year",
                           choices = available_years,
                           selected = max(available_years))
        ),
        column(5,
               selectInput("single_quarter_table_val", "Quarter",
                           choices = c("Q1", "Q2", "Q3", "Q4"),
                           selected = "Q1")
        )
      )
    } else {
      sliderTextInput(
        inputId = "year_range_table",
        label = "Year/Quarter Range",
        choices = quarters_full,
        selected = c(first(quarters_full), last(quarters_full)),
        grid = FALSE,
        dragRange = TRUE,
        force_edges = TRUE
      )
    }
  })
  
  map_agg_prefix <- reactive({
    if (input$year_mode_map == "single") "" else paste0(str_to_title(input$agg_level), " ")
  })
  
  # Helper: get selected quarters for map/barplot
  map_selected_quarters <- reactive({
    if (input$year_mode_map == "single") {
      req(input$single_year_map_val, input$single_quarter_map_val)
      target <- paste0(input$single_year_map_val, ", ", input$single_quarter_map_val)
      quarters_full[quarters_full == target]
    } else {
      req(input$year_range)
      start_idx <- which(quarters_full == input$year_range[1])
      end_idx   <- which(quarters_full == input$year_range[2])
      quarters_full[start_idx:end_idx]
    }
  })
  
  # Helper: get selected quarters for table
  # table_selected_quarters <- reactive({
  #   if (input$year_mode_table == "single") {
  #     req(input$single_year_table_val, input$single_quarter_table_val)
  #     target <- paste0(input$single_year_table_val, ", ", input$single_quarter_table_val)
  #     quarters_full[quarters_full == target]
  #   } else {
  #     req(input$year_range_table)
  #     start_idx <- which(quarters_full == input$year_range_table[1])
  #     end_idx   <- which(quarters_full == input$year_range_table[2])
  #     quarters_full[start_idx:end_idx]
  #   }
  # })
  
  table_selected_quarters <- reactive({
    if (input$year_mode_table == "single") {
      req(input$single_year_table_val, input$single_quarter_table_val)
      target <- paste0(input$single_year_table_val, ", ", input$single_quarter_table_val)
      quarters_full[quarters_full == target]
    } else {
      yr <- input$year_range_table
      if (is.null(yr)) return(quarters_full)  # default to full range on load
      start_idx <- which(quarters_full == yr[1])
      end_idx   <- which(quarters_full == yr[2])
      quarters_full[start_idx:end_idx]
    }
  })
  
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
    
    choices <- sort(na.omit(unique(federal_courts_df_final[[geo_level]])))
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
    selected_quarters <- map_selected_quarters()
    
    federal_courts_df_final %>%
      filter(
        quarter_name_full %in% selected_quarters,
        variables == selected_variable()
      )
  })
  
  filtered_data_full <- reactive({
    selected_quarters <- table_selected_quarters()
    
    if(input$rate_or_count_table == "Per Capita"){
      federal_courts_df_final %>%
        filter(quarter_name_full %in% selected_quarters) %>% 
        dplyr::select(circuit, state_district, state_name, quarter_name_full, values, population, variables, table_num) %>% 
        group_by(quarter_name_full, variables, .data[[input$geo_level_table]], table_num) %>% 
        dplyr::summarize(across(c(values, population),
                                ~.x %>% 
                                  sum(na.rm = TRUE))) %>% 
        ungroup() %>% 
        filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
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
        filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE,
               supervision_type %in% input$supervision_type_table) %>% 
        dplyr::select(quarter_name_full, .data[[input$geo_level_table]], supervision_type, outcome_metric_type, values) %>% 
        pivot_wider(names_from = "outcome_metric_type", 
                    values_from = "values") %>% 
        rename_with(~.x %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title())
    }else{
      federal_courts_df_final %>%
        filter(quarter_name_full %in% selected_quarters) %>% 
        dplyr::select(circuit, state_district, state_name, quarter_name_full, values, variables, table_num) %>% 
        group_by(quarter_name_full, variables, .data[[input$geo_level_table]], table_num) %>% 
        dplyr::summarize(values = 
                           values %>% 
                           sum(na.rm = TRUE)) %>% 
        ungroup() %>% 
        filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
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
        filter(is.na(outcome_metric_type) == FALSE & is.na(supervision_type) == FALSE,
               supervision_type %in% input$supervision_type_table) %>% 
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
        paste0(map_agg_prefix(),
               input$supervision_type, " ",
               input$outcome_type, " per 10,000") %>%
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      if(input$geo_level == "state_name"){
        map_data <- 
          states_sf %>%
          left_join(df_map, by = "state_name")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
        paste0(map_agg_prefix(),
               input$supervision_type, " ",
               input$outcome_type) %>%
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      if(input$geo_level == "state_name"){
        map_data <- 
          states_sf %>%
          left_join(df_map, by = "state_name")
        
        pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
        
        map_data %>% 
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
          filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
      x_lab <- paste0(map_agg_prefix(), input$supervision_type, " ", input$outcome_type, " per 10,000")
      
      df <- 
        agg_data() %>%
        # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
        filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          x = x_lab,
          y = 
            input$geo_level %>%
            str_replace_all("state_name", "State") %>%
            str_replace_all("state_district", "Judicial District") %>%
            str_replace_all("circuit", "Judicial Circuit")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            size = ifelse(input$geo_level == "state_district", 5, 8), 
            margin = margin(r = 0)
          ),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10))
        )
    }else{
      x_lab <- paste0(map_agg_prefix(), input$supervision_type, " ", input$outcome_type)
      
      df <- 
        agg_data() %>%
        # filter(is.na(.data[[input$geo_level_table]]) == FALSE & .data[[input$geo_level_table]] != "Total") %>% 
        filter(is.na(.data[[input$geo_level]]) == FALSE & .data[[input$geo_level]] != "Total") %>% 
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
          x = x_lab,
          y = 
            input$geo_level %>%
            str_replace_all("state_name", "State") %>%
            str_replace_all("state_district", "Judicial District") %>%
            str_replace_all("circuit", "Judicial Circuit")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            size = ifelse(input$geo_level == "state_district", 5, 8), 
            margin = margin(r = 0)
          ), 
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
    
    federal_courts_df_final %>%
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
            group_by(date, supervision_type, outcome_metric_type, quarter_name_full) %>%
            dplyr::summarize(
              values_per_capita = weightedMedian(10000 * (values / population), w = population, na.rm = TRUE),
              values            = weightedMedian(values, w = population, na.rm = TRUE),
              .groups = "drop"
            ) %>%
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
               "Value: ", if(input$rate_or_count_state == "Per Capita") 
                 round(values_per_capita, 2) 
               else 
                 format(round(values, 0), big.mark = ","), "\n",
               "Date: ", format(date, "%b %d, %Y")
             )) %>%
      {
        ggplot(., aes(x = date,
                      y = if(input$rate_or_count_state == "Per Capita") values_per_capita else values,
                      color = state_name,
                      group = state_name,
                      text = hover_text)) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          facet_grid(supervision_type ~ outcome_metric_type, scales = "free_y") +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
          scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          labs(x = "Date", 
               y = if(input$rate_or_count_state == "Per Capita") "Count per 10,000" else "Total Count",
               color = paste0(input$state_selection, " vs. National")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text = element_text(size = 12, face = "bold"),
                strip.text.y = element_text(angle = 0), 
                legend.position = "bottom",
                axis.title.x = element_text(margin = margin(t = 15)),
                axis.title.y = element_text(margin = margin(r = 20)))
      } %>%
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      ),
      margin = list(l = 80, r = 180, t = 40, b = 55))
  })
  
  output$state_viz_stacked_barplot <- renderPlotly({
    req(input$year_range_state)
    
    start_idx <- which(quarters_full == input$year_range_state[1])
    end_idx <- which(quarters_full == input$year_range_state[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    use_per_capita <- input$rate_or_count_state == "Per Capita"
    y_label <- if(use_per_capita) "Mean Count per 10,000 Over Time" else "Mean Count Over Time"
    
    federal_courts_df_final %>%
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
      dplyr::summarize(across(c(values, population), ~.x %>% sum(na.rm = TRUE))) %>% 
      ungroup() %>%
      mutate(values_per_capita = 10000 * (values / population),
             plot_val = if(use_per_capita) values_per_capita else values) %>%
      filter(state_name == input$state_selection & quarter_name_full %in% selected_quarters) %>% 
      group_by(supervision_type, outcome_metric_type, state_name) %>% 
      mutate(plot_val = plot_val %>% mean(na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(supervision_type = 
               supervision_type %>% 
               factor(levels = rev(c("BOP Custody", "Supervised Release", 
                                     "Parole", "Probation", "Total Supervision"))),
             hover_text = paste0(
               "Supervision: ", supervision_type, "\n",
               "Outcome: ", outcome_metric_type, "\n",
               "State: ", state_name, "\n",
               "Mean Value: ", format(round(plot_val, if(use_per_capita) 2 else 0), big.mark = ","), "\n"
             )) %>%
      dplyr::select(state_name, supervision_type, outcome_metric_type, plot_val, hover_text) %>% 
      {
        ggplot(., aes(x = supervision_type, y = plot_val,
                      fill = supervision_type, color = supervision_type,
                      group = supervision_type, text = hover_text)) +
          geom_col(position = "stack", width = 0.99) +
          facet_wrap(~outcome_metric_type) +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_fill_manual(values  = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = y_label, color = "Supervision Type", fill = "Supervision Type") +
          theme_minimal() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                strip.text = element_text(size = 12, face = "bold"),
                legend.position = "bottom",
                axis.title.x = element_text(margin = margin(t = 15)))
      } %>%
      ggplotly(tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top"),
             margin = list(l = 50, r = 180, t = 40, b = 55))
  })
  
  # ---- CHANGE 3: National Dashboard outputs ----
  
  # Shared helper: prepare national base data with labels
  national_base_data <- reactive({
    req(input$year_range_national)
    
    start_idx <- which(quarters_full == input$year_range_national[1])
    end_idx   <- which(quarters_full == input$year_range_national[2])
    selected_quarters <- quarters_full[start_idx:end_idx]
    
    federal_courts_df_final %>%
      mutate(
        outcome_metric_type =
          case_when(
            str_detect(variables, "^received_for_post_conviction_supervision") & table_num == "E-1" ~ "Admissions",
            str_detect(variables, "^removed_from_post_conviction_supervision") & table_num == "E-1" ~ "Exits",
            variables == "persons_under_supervision" & table_num == "E-2" ~ "Total Population",
            str_detect(variables, "^from_") & table_num == "E-2" ~ "Total Population",
            TRUE ~ NA_character_
          ),
        supervision_type =
          case_when(
            str_detect(variables, "bop_custody$")       ~ "BOP Custody",
            str_detect(variables, "supervised_release$") ~ "Supervised Release",
            str_detect(variables, "parole$")             ~ "Parole",
            str_detect(variables, "probation$")          ~ "Probation",
            str_detect(variables, "total_removed$")      ~ "Total Supervision",
            str_detect(variables, "total_received$")     ~ "Total Supervision",
            variables == "persons_under_supervision"     ~ "Total Supervision",
            TRUE ~ NA_character_
          )
      ) %>%
      filter(!is.na(outcome_metric_type), !is.na(supervision_type),
             !is.na(state_name)) %>%
      dplyr::select(state_name, quarter_name_full, values, population,
                    outcome_metric_type, supervision_type) %>%
      filter(quarter_name_full %in% selected_quarters) %>%
      group_by(quarter_name_full, outcome_metric_type, supervision_type) %>%
      dplyr::summarize(
        across(c(values, population), ~sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        values_per_capita = 10000 * (values / population)
      ) %>%
      separate_wider_delim(
        cols = quarter_name_full,
        delim = ", Q",
        names = c("year_chr", "quarter_chr"),
        cols_remove = FALSE
      ) %>%
      mutate(
        quarter_num = as.numeric(quarter_chr),
        month = case_when(
          quarter_num == 1 ~ "03", quarter_num == 2 ~ "06",
          quarter_num == 3 ~ "09", quarter_num == 4 ~ "12"
        ),
        date = as.Date(paste(year_chr, month, "01", sep = "-")),
        date = date + months(1) - days(1),
        supervision_type = factor(
          supervision_type,
          levels = rev(c("BOP Custody", "Supervised Release", "Parole", "Probation", "Total Supervision"))
        )
      )
  })
  
  # National stacked barplot: mean counts over the selected period by supervision type
  output$national_viz_stacked_barplot <- renderPlotly({
    df <- national_base_data()
    
    df %>%
      group_by(supervision_type, outcome_metric_type) %>%
      dplyr::summarize(
        mean_values = mean(values, na.rm = TRUE),
        mean_per_capita = mean(values_per_capita, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        hover_text = paste0(
          "Supervision: ", supervision_type, "\n",
          "Outcome: ", outcome_metric_type, "\n",
          "National Mean Count: ", format(round(mean_values, 0), big.mark = ","), "\n",
          "National Mean per 10k: ", round(mean_per_capita, 2)
        )
      ) %>%
      {
        ggplot(., aes(
          x = supervision_type,
          y = if(input$rate_or_count_national == "Per Capita") mean_per_capita else mean_values,
          fill = supervision_type,
          color = supervision_type,
          text = hover_text
        )) +
          geom_col(position = "stack", width = 0.99) +
          facet_wrap(~outcome_metric_type) +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_fill_manual(values  = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
          scale_y_continuous(labels = scales::comma) +
          labs(
            x = NULL,
            y = if(input$rate_or_count_national == "Per Capita") "National Mean per 10,000 Over Selected Period" else "National Mean Count Over Selected Period",
            color = "Supervision Type",
            fill  = "Supervision Type"
          ) +
          theme_minimal() +
          theme(
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text   = element_text(size = 12, face = "bold"),
            legend.position = "bottom",
            axis.title.x = element_text(margin = margin(t = 15))
          )
      } %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top"),
        margin = list(l = 50, r = 180, t = 40, b = 55)
      )
  })
  
  # National time-series: national totals and per-capita over time, faceted by supervision x outcome
#   output$national_viz_trends <- renderPlotly({
#     df <- national_base_data()
#     
#     df %>%
#       mutate(
#         hover_text = paste0(
#           "Supervision: ", supervision_type, "\n",
#           "Outcome: ", outcome_metric_type, "\n",
#           "National Total: ", format(values, big.mark = ","), "\n",
#           "Per 10k: ", round(values_per_capita, 2), "\n",
#           "Date: ", format(date, "%b %d, %Y")
#         )
#       ) %>%
#       {
#         ggplot(., aes(
#           x = date,
#           y = values_per_capita,
#           color = supervision_type,
#           group = supervision_type,
#           text = hover_text
#         )) +
#           geom_line(linewidth = 1) +
#           geom_point(size = 1) +
#           facet_grid(supervision_type ~ outcome_metric_type, scales = "free_y") +
#           scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")) +
#           scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
#           scale_y_continuous(labels = scales::comma) +
#           labs(
#             x = "Date",
#             y = "National Count per 10,000",
#             color = "Supervision Type",
#             title = "National Supervision Trends Over Time"
#           ) +
#           theme_minimal() +
#           theme(
#             axis.text.x  = element_text(angle = 45, hjust = 1),
#             strip.text   = element_text(size = 12, face = "bold"),
#             strip.text.y = element_text(angle = 0),
#             legend.position = "bottom",
#             axis.title.x = element_text(margin = margin(t = 15)),
#             plot.title   = element_text(size = 14, face = "bold", hjust = 0.5)
#           )
#       } %>%
#       ggplotly(tooltip = "text") %>%
#       layout(
#         legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top"),
#         margin = list(l = 50, r = 180, t = 40, b = 55)
#       )
#   })

  output$national_viz_trends <- renderPlotly({
    df <- national_base_data()
    
    df %>%
      filter(as.character(supervision_type) != "Total Supervision") %>%
      mutate(
        hover_text = paste0(
          "Supervision: ", supervision_type, "\n",
          "Outcome: ", outcome_metric_type, "\n",
          "National Total: ", format(values, big.mark = ","), "\n",
          "Per 10k: ", round(values_per_capita, 2), "\n",
          "Date: ", format(date, "%b %d, %Y")
        )
      ) %>%
      {
        ggplot(., aes(
          x = date,
          y = if(input$rate_or_count_national == "Per Capita") values_per_capita else values,
          color = supervision_type,
          group = supervision_type,
          text = hover_text
        )) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          facet_wrap(~outcome_metric_type, scales = "free_y", ncol = 3) +
          scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7")) +
          scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          scale_y_continuous(labels = scales::comma) +
          labs(
            x = "Date",
            y = if(input$rate_or_count_national == "Per Capita") "National Count per 10,000" else "National Total Count",
            color = "Supervision Type",
            title = "National Supervision Trends Over Time (Excluding Total)"
          ) +
          theme_minimal() +
          theme(
            axis.text.x  = element_text(angle = 45, hjust = 1),
            strip.text   = element_text(size = 12, face = "bold"),
            legend.position = "bottom",
            axis.title.x = element_text(margin = margin(t = 15)),
            plot.title   = element_text(size = 14, face = "bold", hjust = 0.5)
          )
      } %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top"),
        margin = list(l = 50, r = 50, t = 40, b = 55)
      )
  })
}

####Run App####
shinyApp(ui, server)