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
library(patchwork)


####Prep for App####
##Load Pre-processed Data
federal_courts_df_final <- 
  "data" %>% 
  here("federal_courts_df_final.rds") %>% 
  readRDS()

states_sf <- 
  "data" %>% 
  here("states_sf.rds") %>% 
  readRDS()

judicial_district_sf <- 
  "data" %>% 
  here("judicial_district_sf_simplified.rds") %>% 
  readRDS()

judicial_circuit_sf <- 
  "data" %>% 
  here("judicial_circuit_sf_simplified.rds") %>% 
  readRDS()

##Define Theme
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

##Define Quarters
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

##Helper Functions
carto_key <- Sys.getenv("CARTO_API_KEY")   # set in .Renviron or your host's env vars

add_basemap <- function(map){
  if(nzchar(carto_key)){
    addTiles(
      map,
      urlTemplate = paste0(
        "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key=",
        carto_key
      ),
      attribution = paste0(
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors ',
        '&copy; <a href="https://carto.com/attributions">CARTO</a>'
      )
    )
  } else {
    # keyless fallback so the map never shows a watermark
    addProviderTiles(map, "Esri.WorldGrayCanvas")
  }
}


####Overview and About Page Content####
##Formatting Helpers
fmt_date_long  <- function(d) paste0(format(d, "%B "), as.integer(format(d, "%d")), ", ", format(d, "%Y"))
fmt_month_year <- function(d) format(d, "%B %Y")
fmt_int        <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_rate       <- function(x) formatC(x, format = "f", digits = 2)
fmt_pct        <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_list       <- function(x){
  x <- as.character(x)
  if(length(x) <= 1) return(x)
  if(length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}
fmt_q          <- function(x) str_replace_all(x, "^([0-9]{4}), (Q[1-4])$", "\\2 \\1")   # "2025, Q4" -> "Q4 2025"
hex_alpha <- function(hex, alpha){
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

##Pre-compute National and State Figures (runs once at app start-up)
# Uses the same outcome / supervision-type definitions as the dashboards above,
# so every number on the Overview page matches what the dashboards show.
ov_base <-
  federal_courts_df_final %>%
  mutate(outcome_metric_type =
           case_when(str_detect(variables, "^received_for_post_conviction_supervision") & table_num == "E-1" ~ "Admissions",
                     str_detect(variables, "^removed_from_post_conviction_supervision") & table_num == "E-1" ~ "Exits",
                     variables == "persons_under_supervision" & table_num == "E-2" ~ "Total Population",
                     str_detect(variables, "^from_") & table_num == "E-2" ~ "Total Population",
                     TRUE ~ NA_character_),
         supervision_type =
           case_when(str_detect(variables, "bop_custody$") ~ "BOP Custody",
                     str_detect(variables, "supervised_release$") ~ "Supervised Release",
                     str_detect(variables, "parole$") ~ "Parole",
                     str_detect(variables, "probation$") ~ "Probation",
                     str_detect(variables, "total_removed$") ~ "Total Supervision",
                     str_detect(variables, "total_received$") ~ "Total Supervision",
                     variables == "persons_under_supervision" ~ "Total Supervision",
                     TRUE ~ NA_character_)) %>%
  filter(!is.na(outcome_metric_type), !is.na(supervision_type), !is.na(state_name)) %>%
  dplyr::select(date = date_full, quarter_name_full, state_name, outcome_metric_type, supervision_type, values, population)

ov_nat <-
  ov_base %>%
  group_by(date, quarter_name_full, outcome_metric_type, supervision_type) %>%
  dplyr::summarize(across(c(values, population), ~.x %>% sum(na.rm = TRUE)), .groups = "drop") %>%
  mutate(per_10k = 10000 * (values / population))

ov_pop    <- ov_nat %>% filter(outcome_metric_type == "Total Population")
ov_total  <- ov_pop %>% filter(supervision_type == "Total Supervision") %>% arrange(date)
ov_latest <- ov_total %>% slice_max(date, n = 1, with_ties = FALSE)
ov_first  <- ov_total %>% slice_min(date, n = 1, with_ties = FALSE)
ov_peak   <- ov_total %>% slice_max(values, n = 1, with_ties = FALSE)

ov_pct_vs_peak   <- ov_latest$values / ov_peak$values - 1
ov_rate_vs_peak  <- ov_latest$per_10k / ov_peak$per_10k - 1

# Share of the supervised population by supervision type
ov_share <-
  ov_pop %>%
  filter(supervision_type != "Total Supervision") %>%
  group_by(date) %>%
  mutate(share = values / sum(values, na.rm = TRUE)) %>%
  ungroup()
ov_share_at <- function(d, type) ov_share %>% filter(date == d, supervision_type == type) %>% pull(share)

# Admissions and exits (12-month totals ending on each date)
ov_flow <-
  ov_nat %>%
  filter(outcome_metric_type %in% c("Admissions", "Exits"), supervision_type == "Total Supervision") %>%
  dplyr::select(date, outcome_metric_type, values) %>%
  pivot_wider(names_from = outcome_metric_type, values_from = values) %>%
  arrange(date)

# One row for every reporting date, so quarters that were excluded show up as gaps in the line
ov_flow_grid <-
  ov_total %>%
  dplyr::select(date, quarter_name_full) %>%
  left_join(ov_flow, by = "date")

ov_flow_latest <- ov_flow %>% filter(!is.na(Admissions), !is.na(Exits)) %>% slice_max(date, n = 1, with_ties = FALSE)
ov_adm_peak    <- ov_flow %>% filter(!is.na(Admissions)) %>% slice_max(Admissions, n = 1, with_ties = FALSE)
ov_exit_window <- ov_flow %>% filter(!is.na(Exits), date > ov_flow_latest$date - lubridate::years(10))
ov_exit_lo     <- round(min(ov_exit_window$Exits) / 500) * 500
ov_exit_hi     <- round(max(ov_exit_window$Exits) / 500) * 500
ov_exit_from   <- lubridate::year(min(ov_exit_window$date))
ov_adm_vs_peak <- ov_flow_latest$Admissions / ov_adm_peak$Admissions - 1
ov_net_latest  <- ov_flow_latest$Admissions - ov_flow_latest$Exits

ov_flow_missing <-
  ov_flow_grid %>%
  filter(is.na(Exits) | is.na(Admissions)) %>%
  pull(quarter_name_full) %>%
  str_replace_all("^([0-9]{4}), (Q[1-4])$", "\\2 \\1")

# State and territory rates on the latest reporting date
ov_territories <- c("Puerto Rico", "Guam", "Virgin Islands", "Northern Mariana Islands")
ov_state <-
  ov_base %>%
  filter(outcome_metric_type == "Total Population", supervision_type == "Total Supervision", date == ov_latest$date) %>%
  group_by(state_name) %>%
  dplyr::summarize(across(c(values, population), ~.x %>% sum(na.rm = TRUE)), .groups = "drop") %>%
  filter(population > 0) %>%
  mutate(per_10k = 10000 * (values / population),
         label = ifelse(state_name %in% ov_territories, paste0(state_name, "*"), state_name)) %>%
  arrange(desc(per_10k))

ov_n_geo_show <- 8
ov_geo_plot <-
  bind_rows(ov_state %>% slice_head(n = ov_n_geo_show) %>% mutate(group = "Highest rates"),
            ov_state %>% slice_tail(n = ov_n_geo_show) %>% mutate(group = "Lowest rates"))

ov_top_count <- ov_state %>% slice_max(values, n = 5, with_ties = FALSE)
ov_top_count_share <- sum(ov_top_count$values) / sum(ov_state$values)
ov_rate_hi   <- ov_state %>% slice_head(n = 1)
ov_rate_lo   <- ov_state %>% slice_tail(n = 1)
ov_rate_fold <- ov_rate_hi$per_10k / ov_rate_lo$per_10k

# Geography and coverage counts
ov_n_dates     <- n_distinct(ov_total$date)
ov_n_districts <- n_distinct(federal_courts_df_final$state_district, na.rm = TRUE)
ov_n_states    <- n_distinct(ov_base$state_name)
ov_n_circuits  <-
  federal_courts_df_final %>%
  filter(!is.na(state_name), !is.na(circuit), circuit != "Total") %>%
  pull(circuit) %>%
  n_distinct()

# Last year in which the population denominator changed (later dates reuse that estimate)
ov_pop_last_year <-
  federal_courts_df_final %>%
  filter(!is.na(state_district), !is.na(population)) %>%
  distinct(year, state_district, population) %>%
  group_by(year) %>%
  dplyr::summarize(population = sum(population), .groups = "drop") %>%
  arrange(year) %>%
  mutate(changed = population != lag(population)) %>%
  filter(changed) %>%
  pull(year) %>%
  max()

# Coverage of the tables that are in the data but not yet charted in the dashboards
ov_other_tables <-
  federal_courts_df_final %>%
  filter(table_num %in% c("E-3", "E-8", "E-8A", "E-10")) %>%
  group_by(table_num) %>%
  dplyr::summarize(first_date = min(date_full, na.rm = TRUE),
                   last_date  = max(date_full, na.rm = TRUE),
                   n_dates    = n_distinct(date_full),
                   .groups = "drop")
ov_other_cov <- function(tn){
  r <- ov_other_tables %>% filter(table_num == tn)
  if(nrow(r) == 0) return("not in this data release")
  if(r$n_dates == 1) return(fmt_month_year(r$first_date))
  paste0(r$n_dates, " dates, ", fmt_month_year(r$first_date), " to ", fmt_month_year(r$last_date))
}

# Source file formats (older tables were PDFs, newer ones are Excel files)
ov_file_types <-
  federal_courts_df_final %>%
  filter(table_num %in% c("E-1", "E-2")) %>%
  group_by(file_type) %>%
  dplyr::summarize(last_date = max(date_full, na.rm = TRUE), .groups = "drop")
ov_pdf_last <- ov_file_types %>% filter(file_type == "PDF") %>% pull(last_date)

##Overview Charts
ov_type_cols <- c("Supervised Release" = "#009E73", "Probation" = "#0072B2", "Parole" = "#E69F00", "BOP Custody" = "#CC79A7")

ov_chart_style <- function(p){
  p %>%
    layout(font = list(family = "Roboto, sans-serif", size = 13, color = "#2C2C2C"),
           paper_bgcolor = "rgba(0,0,0,0)",
           plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)
}

ov_plot_pop <- function(){
  p <- plot_ly()
  
  # Invisible total line so the hover box also shows the all-types total
  p <-
    p %>%
    add_trace(data = ov_total, x = ~date, y = ~values, type = "scatter", mode = "lines",
              name = "Total", showlegend = FALSE,
              line = list(color = "rgba(0,0,0,0)", width = 0),
              hovertemplate = "%{y:,.0f}<extra>Total</extra>")
  
  for(tp in names(ov_type_cols)){
    p <-
      p %>%
      add_trace(data = ov_pop %>% filter(supervision_type == tp) %>% arrange(date),
                x = ~date, y = ~values, type = "scatter", mode = "lines",
                stackgroup = "one", name = tp,
                line = list(color = ov_type_cols[[tp]], width = 0.8),
                fillcolor = hex_alpha(ov_type_cols[[tp]], 0.75),
                hovertemplate = paste0("%{y:,.0f}<extra>", tp, "</extra>"))
  }
  
  p %>%
    layout(hovermode = "x unified",
           xaxis = list(title = "", type = "date", hoverformat = "%b %d, %Y", showgrid = FALSE),
           yaxis = list(title = "People under supervision", tickformat = ",d",
                        range = c(0, ov_peak$values * 1.2), gridcolor = "#E6DCC3", zeroline = FALSE),
           legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top", traceorder = "normal"),
           margin = list(l = 75, r = 20, t = 20, b = 60),
           annotations = list(list(x = as.character(ov_peak$date), y = ov_peak$values,
                                   text = paste0("Peak: ", fmt_int(ov_peak$values), "<br>", format(ov_peak$date, "%b %Y")),
                                   showarrow = TRUE, arrowhead = 2, arrowsize = 0.8, ax = -45, ay = -35,
                                   font = list(size = 12), bgcolor = "rgba(255,255,255,0.85)"))) %>%
    ov_chart_style()
}

ov_plot_flow <- function(){
  y_min <- floor(min(c(ov_flow$Admissions, ov_flow$Exits), na.rm = TRUE) * 0.9 / 5000) * 5000
  y_max <- ceiling(max(c(ov_flow$Admissions, ov_flow$Exits), na.rm = TRUE) * 1.05 / 5000) * 5000
  
  plot_ly(ov_flow_grid, x = ~date) %>%
    add_trace(y = ~Admissions, type = "scatter", mode = "lines+markers", name = "Admissions",
              connectgaps = FALSE, line = list(color = "#0072B2", width = 2),
              marker = list(color = "#0072B2", size = 4),
              hovertemplate = "%{y:,.0f}<extra>Admissions</extra>") %>%
    add_trace(y = ~Exits, type = "scatter", mode = "lines+markers", name = "Exits",
              connectgaps = FALSE, line = list(color = "#D55E00", width = 2),
              marker = list(color = "#D55E00", size = 4),
              hovertemplate = "%{y:,.0f}<extra>Exits</extra>") %>%
    layout(hovermode = "x unified",
           xaxis = list(title = "", type = "date", hoverformat = "%b %d, %Y", showgrid = FALSE),
           yaxis = list(title = "People (12-month total)", tickformat = ",d",
                        range = c(y_min, y_max), gridcolor = "#E6DCC3", zeroline = FALSE),
           legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top"),
           margin = list(l = 75, r = 20, t = 20, b = 60)) %>%
    ov_chart_style()
}

ov_plot_geo <- function(){
  nat_rate <- ov_latest$per_10k
  
  d <-
    ov_geo_plot %>%
    mutate(hover = paste0("<b>", label, "</b><br>", fmt_rate(per_10k), " per 10,000 residents<br>",
                          fmt_int(values), " people under supervision"))
  
  # Blank spacer row separates the highest and lowest groups
  spacer <- data.frame(label = " ", per_10k = NA_real_, hover = "", group = "Spacer", stringsAsFactors = FALSE)
  d_all <-
    bind_rows(d %>% filter(group == "Highest rates"), spacer, d %>% filter(group == "Lowest rates")) %>%
    mutate(label = factor(label, levels = rev(label)))
  
  grp_cols <- c("Highest rates" = "#009E73", "Lowest rates" = "#0072B2")
  
  p <- plot_ly()
  for(g in names(grp_cols)){
    p <-
      p %>%
      add_trace(data = d_all %>% filter(group == g), x = ~per_10k, y = ~label, type = "bar", orientation = "h",
                name = g, marker = list(color = grp_cols[[g]]),
                customdata = ~hover, hovertemplate = "%{customdata}<extra></extra>",
                texttemplate = "%{x:.2f}", textposition = "outside", cliponaxis = FALSE)
  }
  
  p %>%
    layout(barmode = "overlay",
           xaxis = list(title = "People under supervision per 10,000 residents", range = c(0, max(ov_geo_plot$per_10k) * 1.15),
                        gridcolor = "#E6DCC3", zeroline = FALSE),
           yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = levels(d_all$label)),
           legend = list(orientation = "h", x = 0, y = -0.2),
           margin = list(l = 10, r = 30, t = 30, b = 70),
           shapes = list(list(type = "line", x0 = nat_rate, x1 = nat_rate, y0 = 0, y1 = 1, yref = "paper",
                              line = list(color = "#2C2C2C", width = 1.5, dash = "dash"))),
           annotations = list(list(x = nat_rate, y = 0.02, yref = "paper", xanchor = "left", yanchor = "bottom",
                                   text = paste0("National: ", fmt_rate(nat_rate)), showarrow = FALSE, xshift = 5,
                                   font = list(size = 12), bgcolor = "rgba(255,255,255,0.85)"))) %>%
    ov_chart_style()
}


##Overview Page (landing page)
ov_year_first   <- lubridate::year(ov_first$date)
ov_year_latest  <- lubridate::year(ov_latest$date)
ov_at_peak      <- abs(ov_pct_vs_peak) < 0.0005
dir_word        <- function(now, then, up, down) ifelse(now >= then, up, down)

ab_link_html <- function(href, label){
  paste0('<a href="', href, '" target="_blank" rel="noopener noreferrer">', label, '</a>')
}

ov_kpi <- function(label, value, note){
  div(class = "ov-kpi",
      div(class = "ov-kpi-label", label),
      div(class = "ov-kpi-value", value),
      div(class = "ov-kpi-note", note))
}

ov_trend_block <- function(text, plot_id, caption, height = 380){
  div(class = "ov-trend",
      div(class = "ov-trend-text", text),
      div(class = "ov-trend-chart",
          plotlyOutput(plot_id, height = height, width = "100%"),
          div(class = "ov-caption", caption)))
}

ov_link_card <- function(id, title, desc){
  actionLink(inputId = id,
             label = tagList(span(class = "t", title), span(class = "d", desc)),
             class = "ov-card-link")
}

# Trend 1 and 2 text (population level and mix of supervision types)
ov_t1_title <-
  if(ov_at_peak){
    "The supervised population is at its highest point in the data."
  }else{
    paste0("Supervision peaked in ", fmt_month_year(ov_peak$date), " and is now ", fmt_pct(abs(ov_pct_vs_peak)),
           ifelse(ov_pct_vs_peak < 0, " lower.", " higher."))
  }

ov_t1_body <-
  paste0("On ", fmt_date_long(ov_latest$date), ", ", fmt_int(ov_latest$values),
         " people were under federal post-conviction supervision",
         if(ov_at_peak) "." else paste0(", ", fmt_pct(abs(ov_pct_vs_peak)), " ", ifelse(ov_pct_vs_peak < 0, "below", "above"),
                                        " the ", fmt_month_year(ov_peak$date), " high of ", fmt_int(ov_peak$values), "."),
         " ",
         ifelse(ov_rate_vs_peak < ov_pct_vs_peak,
                "Because the U.S. population kept growing, the per-capita decline is steeper: ",
                "Per 10,000 residents, that is "),
         fmt_rate(ov_latest$per_10k), " people per 10,000 residents, versus ",
         fmt_rate(ov_peak$per_10k), " at the peak and ", fmt_rate(ov_first$per_10k), " in ", ov_year_first, ".")

ov_sr_now <- ov_share_at(ov_latest$date, "Supervised Release"); ov_sr_first <- ov_share_at(ov_first$date, "Supervised Release")
ov_pb_now <- ov_share_at(ov_latest$date, "Probation");          ov_pb_first <- ov_share_at(ov_first$date, "Probation")
ov_pa_now <- ov_share_at(ov_latest$date, "Parole")

ov_t2_title <-
  ifelse(ov_sr_now > ov_sr_first,
         "The mix has shifted toward supervised release.",
         "The mix of supervision types has changed.")

ov_t2_body <-
  paste0("Supervised release, which follows a prison term, accounts for ", fmt_pct(ov_sr_now),
         " of the supervised population, ", dir_word(ov_sr_now, ov_sr_first, "up from ", "down from "),
         fmt_pct(ov_sr_first), " in ", ov_year_first, ". Probation, a sentence served in the community instead of prison, ",
         "has ", dir_word(ov_pb_now, ov_pb_first, "risen", "dropped"), " from ", fmt_pct(ov_pb_first), " to ", fmt_pct(ov_pb_now),
         ", and parole is now ", fmt_pct(ov_pa_now), ", since federal parole was abolished for offenses committed after 1987.")

# Trend 3 text (admissions and exits)
ov_t3_title <-
  ifelse(ov_adm_vs_peak < -0.05 & (ov_exit_hi - ov_exit_lo) / ov_exit_hi < 0.1,
         "Fewer people are entering supervision, while exits have held steady.",
         "Admissions and exits drive changes in the supervised population.")

ov_t3_body <-
  paste0("In the 12 months ending ", fmt_date_long(ov_flow_latest$date), ", ", fmt_int(ov_flow_latest$Admissions),
         " people were admitted to supervision and ", fmt_int(ov_flow_latest$Exits), " left it, a net ",
         ifelse(ov_net_latest < 0, "decrease", "increase"), " of ", fmt_int(abs(ov_net_latest)), ". ",
         if(abs(ov_adm_vs_peak) < 0.0005){
           paste0("Admissions are at their high of ", fmt_int(ov_adm_peak$Admissions), ", ")
         }else{
           paste0("Admissions are ", fmt_pct(abs(ov_adm_vs_peak)), " below their ", fmt_month_year(ov_adm_peak$date),
                  " high of ", fmt_int(ov_adm_peak$Admissions), ", ")
         },
         "while annual exits have stayed between roughly ", fmt_int(ov_exit_lo), " and ", fmt_int(ov_exit_hi),
         " since ", ov_exit_from, ". Both counts include transfers between districts.")

# Trend 4 text (geography)
ov_t4_title <- paste0("Supervision rates differ about ", round(ov_rate_fold), "-fold across states and territories.")

ov_t4_body <-
  paste0("On ", fmt_date_long(ov_latest$date), ", rates ranged from ", fmt_rate(ov_rate_lo$per_10k), " per 10,000 residents in ",
         ov_rate_lo$state_name, " to ", fmt_rate(ov_rate_hi$per_10k), " in ", ov_rate_hi$state_name,
         ", against a national rate of ", fmt_rate(ov_latest$per_10k), ". By raw count, ", fmt_list(ov_top_count$state_name),
         " lead and together account for ", fmt_pct(ov_top_count_share), " of everyone under supervision. Per capita, the highest rates are in ",
         fmt_list(head(ov_state$state_name, 3)), ". Rates reflect many factors, including federal enforcement priorities and the mix of cases ",
         "brought in each district, so they show where supervision is concentrated, not why. Small populations can also make rates volatile.")

ov_reading_notes <-
  tags$ul(
    tags$li(HTML("<strong>Snapshots vs. flows.</strong> Population figures are headcounts on a date. Admissions and exits are totals for the 12 months ending on that date.")),
    tags$li(HTML(paste0("<strong>Per-capita rates</strong> divide by total resident population (all ages) from Census Bureau estimates",
                        ifelse(ov_pop_last_year < ov_year_latest,
                               paste0(". The latest estimate is for ", ov_pop_last_year, ", so later dates reuse it."),
                               ".")))),
    tags$li(HTML(paste0("<strong>Gaps.</strong> Admissions and exits are blank for ", fmt_list(ov_flow_missing),
                        ", where district figures did not match national totals.")))
  )

overview_ui <-
  div(class = "ov-page",
      
      # Hero: what this is
      div(class = "ov-hero",
          h1(HTML(paste0("Federal post-conviction supervision, ", ov_year_first, "&ndash;", ov_year_latest))),
          p(class = "ov-lede",
            "People sentenced in federal court may be supervised in the community by U.S. probation officers: on probation instead of prison, ",
            "or on supervised release after a prison term. This dashboard shows how many people are under federal supervision, ",
            "how many enter and leave it, and how all of that varies across the country."),
          p(class = "ov-body",
            HTML(paste0("The figures come from the federal judiciary's Federal Probation System tables (E-1 and E-2) on ",
                        ab_link_html("https://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables", "uscourts.gov"),
                        ", covering ", ov_n_dates, " reporting dates from ", fmt_q(ov_first$quarter_name_full), " to ", fmt_q(ov_latest$quarter_name_full),
                        " across ", ov_n_districts, " federal judicial districts. The dashboards roll districts up into ", ov_n_circuits,
                        " circuits and ", ov_n_states, " states and territories, and can express counts per 10,000 residents ",
                        "(using Census Bureau population estimates) so that large and small places can be compared fairly."))),
          span(class = "ov-asof", paste0("Data through ", fmt_date_long(ov_latest$date)))),
      
      # Headline numbers
      div(class = "ov-kpis",
          ov_kpi("Under supervision", fmt_int(ov_latest$values), paste0("as of ", fmt_date_long(ov_latest$date))),
          ov_kpi("Per 10,000 residents", fmt_rate(ov_latest$per_10k),
                 paste0(fmt_rate(ov_first$per_10k), " in ", ov_year_first, "; high of ", fmt_rate(ov_peak$per_10k), " in ", fmt_month_year(ov_peak$date))),
          ov_kpi("Change since the peak",
                 if(ov_at_peak) "At peak" else HTML(paste0(ifelse(ov_pct_vs_peak < 0, "&minus;", "+"), fmt_pct(abs(ov_pct_vs_peak)))),
                 paste0("peak was ", fmt_int(ov_peak$values), " in ", fmt_month_year(ov_peak$date))),
          ov_kpi("On supervised release", fmt_pct(ov_sr_now),
                 paste0(dir_word(ov_sr_now, ov_sr_first, "up from ", "down from "), fmt_pct(ov_sr_first), " in ", ov_year_first))),
      
      # Key trends
      h2(class = "ov-section-title", "Key trends"),
      p(class = "ov-section-sub",
        paste0("Four patterns stand out in the national data through ", fmt_q(ov_latest$quarter_name_full),
               ". Every number and chart here is built from the same data as the dashboards.")),
      
      ov_trend_block(
        tagList(h3(ov_t1_title), p(ov_t1_body), h3(ov_t2_title), p(ov_t2_body)),
        "ov_pop_chart",
        "People under supervision on each reporting date, by supervision type (Table E-2). Reporting is annual (September 30) through 2000 and quarterly from 2001.",
        height = 420),
      
      ov_trend_block(
        tagList(h3(ov_t3_title), p(ov_t3_body)),
        "ov_flow_chart",
        paste0("People admitted to and removed from supervision during the 12 months ending on each date (Table E-1), including transfers between districts. ",
               "Gaps mark dates excluded because district figures did not match national totals. The vertical axis does not start at zero."),
        height = 380),
      
      ov_trend_block(
        tagList(h3(ov_t4_title), p(ov_t4_body)),
        "ov_geo_chart",
        paste0("People under supervision per 10,000 residents on ", fmt_date_long(ov_latest$date), ": the ", ov_n_geo_show, " highest and ", ov_n_geo_show,
               " lowest states and territories. * = territory. The dashed line is the national rate."),
        height = 520),
      
      # Where to go next
      h2(class = "ov-section-title", "Explore the dashboards"),
      div(class = "ov-explore",
          ov_link_card("ov_go_main", "Main Dashboard", "Map, ranked bars and a downloadable table for any state, district or circuit."),
          ov_link_card("ov_go_state", "State Dashboard", "Pick a state and compare its trends with the national picture."),
          ov_link_card("ov_go_national", "National Dashboard", "National trends in population, admissions and exits by supervision type."),
          ov_link_card("ov_go_about", "About and data dictionary", "How to use the tool, key terms, definitions and caveats.")),
      
      # Reading notes
      div(class = "ov-notes",
          h3("Reading these numbers"),
          ov_reading_notes,
          p("See the About tab for full definitions and caveats."))
  )


##About Page
ab_card <- function(title, where, steps, tip = NULL, caution = NULL){
  div(class = "ab-card",
      h3(title),
      div(class = "ab-where", HTML(where)),
      tags$ol(lapply(steps, function(s) tags$li(HTML(s)))),
      if(!is.null(tip)) div(class = "ab-tip", HTML(paste0("<strong>Tip:</strong> ", tip))),
      if(!is.null(caution)) div(class = "ab-caution", HTML(paste0("<strong>Watch out:</strong> ", caution))))
}

ab_table <- function(df, code_cols = character(0)){
  div(class = "ab-table-wrap",
      tags$table(class = "table table-sm ab-table",
                 tags$thead(tags$tr(lapply(names(df), tags$th))),
                 tags$tbody(lapply(seq_len(nrow(df)), function(i){
                   tags$tr(lapply(names(df), function(nm){
                     val <- df[[nm]][i]
                     tags$td(if(nm %in% code_cols) tags$code(val) else HTML(val))
                   }))
                 }))))
}

ab_term_group <- function(title, terms){
  div(class = "ab-term-group",
      h3(title),
      lapply(terms, function(t){
        tags$dl(class = "ab-term", tags$dt(t[[1]]), tags$dd(HTML(t[[2]])))
      }))
}

##Ways to use the tool
ab_use_cards <-
  div(class = "ab-card-grid",
      
      ab_card("See where supervision is concentrated", "Main Dashboard &rsaquo; Map and Barplot Filters",
              c("Set <strong>Outcome Metric Type</strong> to Population and <strong>Supervision Type</strong> to Total Supervision.",
                "Choose <strong>Per Capita</strong> to compare places of different sizes, or <strong>Total Count</strong> to see where the most people are.",
                "Pick a <strong>Geography Level</strong> (State, Judicial District or Judicial Circuit). The map and the ranked bar chart update together.",
                "Hover over the map or the bars for exact values."),
              tip = "Counts mostly follow population, so large states lead. Per-capita rates bring out smaller places with unusually high supervision."),
      
      ab_card("Compare one state with the nation over time", "State Dashboard",
              c("Choose a <strong>State</strong>, then <strong>Count or Per Capita</strong> and a <strong>Year Range</strong>.",
                "The top chart shows the average level of each outcome (population, admissions, exits) for each supervision type across your date range.",
                "The grid below plots your state's trend against the <strong>National Mean</strong> and <strong>National Median</strong>."),
              tip = "The National Mean is a simple average across states and territories, so a small territory counts as much as Texas. The National Median is weighted by population, so it reflects where the typical resident lives."),
      
      ab_card("Follow national trends", "National Dashboard",
              c("Choose <strong>Count or Per Capita</strong> and a <strong>Year Range</strong>.",
                "The upper chart tracks the supervised population, admissions and exits over time for each supervision type. The Total Supervision line is left out of this chart.",
                "The lower chart compares average levels across supervision types for the period you selected."),
              tip = "Per capita removes the effect of population growth, which helps when comparing years that are far apart."),
      
      ab_card("Pull numbers for a report", "Main Dashboard &rsaquo; Table Filters",
              c("Select one or more <strong>Supervision Types</strong>, then <strong>Count or Per Capita</strong> and a <strong>Geography Level</strong>.",
                "Optionally narrow the table to specific states, districts or circuits with the geography filter that appears below Geography Level.",
                "Set the dates (a range or a single quarter). Click column headers to sort, use the boxes at the top of each column to search, and use <strong>Copy</strong>, <strong>CSV</strong> or <strong>Excel</strong> to export."),
              tip = "Each row is one place, one date and one supervision type, with Admissions, Exits and Total Population side by side."),
      
      ab_card("Compare two moments in time", "Main Dashboard",
              c("Set <strong>Date Selection Mode</strong> to Single Year/Quarter and choose a year and quarter.",
                "Note the map and bars, then switch to a second date and compare.",
                "For the average over many dates, use Year/Quarter Range instead."),
              tip = "A single-date view shows each place's figure for that date with no averaging across quarters."),
      
      ab_card("Look at admissions and exits", "Main Dashboard &rsaquo; Map and Barplot Filters",
              c("Set <strong>Outcome Metric Type</strong> to Admissions or Exits, then choose a supervision type.",
                "To read an annual figure, use a single date (a Q4 date gives calendar-year totals) or the <strong>Mean</strong> aggregation."),
              tip = "Admissions and exits include transfers between districts, so at the district level they overstate new entries and departures.",
              caution = "<strong>Sum</strong>, the default for Admissions and Exits counts, adds up every quarter in your range. Each quarter already covers 12 months and consecutive quarters overlap, so a multi-quarter sum counts the same people more than once."),
      
      ab_card("Zoom in on your own district", "Main Dashboard",
              c("Set <strong>Geography Level</strong> to Judicial District. District names combine the state and the division, such as Texas Northern or California Central. States with a single district use the state name.",
                "For a table of specific districts, open <strong>Table Filters</strong>, set Geography Level to Judicial District, and pick districts in the filter that appears."),
              tip = "Districts follow federal court boundaries, which nest inside states. Each district's population is built up from county estimates.")
  )

##Key terms
ab_glossary <-
  tagList(
    div(class = "ab-glossary-search",
        tags$input(type = "search", id = "glossary-filter", class = "form-control",
                   placeholder = "Filter terms (for example: parole, transfers, per 10,000)", `aria-label` = "Filter key terms")),
    
    ab_term_group("Supervision", list(
      list("Post-conviction supervision", "Community supervision, by a U.S. probation officer, of a person convicted of a federal offense. It covers probation, supervised release, parole and the BOP custody category described below. These tables do not cover pretrial supervision of people awaiting trial."),
      list("Probation", "A sentence served in the community instead of a prison term. In Table E-2 these people are counted as coming from the courts. The category includes conditional release and the former judge and magistrate judge probation."),
      list("Supervised release", "A period of supervision that follows a prison term. In Table E-2 these people are counted as coming from institutions (prisons)."),
      list("Parole", "Early release from prison under the supervision of the U.S. Parole Commission. Federal parole was abolished for offenses committed after November 1, 1987, so the category is now small and shrinking. The tables group parole, special parole, mandatory release and military parole together."),
      list("BOP custody", "A small category of people who remain in the custody of the Federal Bureau of Prisons (BOP) while being supervised in the community. The courts' table footnotes say that, effective January 26, 2020, it covers BOP Federal Location Monitoring and Elderly Home Confinement."),
      list("U.S. probation officer", "Court employees in each district's probation office who supervise people on probation, supervised release and parole, monitor compliance with the conditions set by the court, and help people return to the community.")
    )),
    
    ab_term_group("Measures", list(
      list("Total Population (persons under supervision)", "A headcount of people under supervision on a given date, like a snapshot. It is labeled Total Population in the dashboards and comes from Table E-2."),
      list("Admissions (persons received)", "People who began supervision, or whose supervision was transferred in from another district, during the 12 months ending on a date. From Table E-1."),
      list("Exits (persons removed)", "People whose supervision ended, or was transferred out to another district, during the 12 months ending on a date. From Table E-1."),
      list("Transfers", "Supervision cases moved from one district to another. Admissions and exits both include transfers, so a transfer appears as an exit in one district and an admission in another. Nationally they roughly offset, but district and state figures overstate new entries and departures."),
      list("Total Supervision", "The all-types figure in the dashboards. For Total Population it is everyone under supervision, and the four supervision types add up to it. For Admissions and Exits it is the courts' total received or removed, which includes transfers, so it is larger than the sum of the four types."),
      list("Net change", "Admissions minus exits over the same 12 months. Positive means the supervised population is roughly growing, negative means it is roughly shrinking. Used on the Overview page and not charted separately."),
      list("Per 10,000 (per capita)", "A count divided by the resident population of the area and multiplied by 10,000, so places of different sizes can be compared. It uses the total resident population of all ages, not just adults."),
      list("Total Count", "The raw number of people, with no adjustment for population.")
    )),
    
    ab_term_group("Geography and time", list(
      list("Judicial district", paste0("A federal trial-court jurisdiction. The data include ", ov_n_districts, " districts. Each state has between one and four, and the District of Columbia, Puerto Rico, Guam, the Virgin Islands and the Northern Mariana Islands have one each.")),
      list("Judicial circuit", paste0("A regional group of districts under a U.S. Court of Appeals. The data cover the 1st through 11th Circuits and the D.C. Circuit (", ov_n_circuits, " in all).")),
      list("State (in the dashboards)", paste0("The State level includes the 50 states, the District of Columbia and four territories (Puerto Rico, Guam, the Virgin Islands and the Northern Mariana Islands): ", ov_n_states, " in all.")),
      list("Quarter labels", "A label such as 2025, Q4 refers to the quarter ending on a date: Q1 is March 31, Q2 is June 30, Q3 is September 30 and Q4 is December 31. Reporting is annual (September 30) from 1997 to 2000 and quarterly from 2001."),
      list("12-month reporting period", "Admissions and exits cover the 12 months ending on the selected date, so consecutive quarters overlap by nine months. A Q4 date gives calendar-year totals.")
    )),
    
    ab_term_group("Dashboard controls", list(
      list("Aggregation Type", "How the quarterly values in your date range are combined into one number per place: Mean (average), Median (middle value), Minimum, Maximum, or Sum (a total, offered for Admissions and Exits counts). In single-date mode there is only one quarter, so nothing is aggregated. For per-capita figures, the Mean divides total people across the period by total population across the period, and the Median is weighted by population."),
      list("National Mean and National Median", "On the State Dashboard, the National Mean is the simple average across states and territories, and the National Median is the population-weighted median. Both are calculated separately for each date."),
      list("Date Selection Mode", "On the Main Dashboard, chooses between a range of quarters (Year/Quarter Range) and one quarter (Single Year/Quarter).")
    ))
  )

##Data dictionary
ab_dict_measures <-
  tribble(
    ~`Dashboard measure`, ~`Supervision type`, ~`Source table and line`, ~`What it counts`, ~`Time basis`,
    "Total Population", "Total Supervision", "E-2: Persons under supervision", "Everyone under post-conviction supervision", "Headcount on the reporting date",
    "Total Population", "Probation", "E-2: From courts, probation", "People serving a sentence of probation (includes conditional release)", "Headcount on the reporting date",
    "Total Population", "Supervised Release", "E-2: From institutions, term of supervised release", "People on supervised release after a prison term", "Headcount on the reporting date",
    "Total Population", "Parole", "E-2: From institutions, parole", "People on parole, special parole, mandatory release or military parole", "Headcount on the reporting date",
    "Total Population", "BOP Custody", "E-2: From institutions, BOP custody", "People in Bureau of Prisons custody who are supervised in the community", "Headcount on the reporting date",
    "Admissions", "Total Supervision", "E-1: Received for post-conviction supervision, total received", "All people received, including transfers in from other districts", "12 months ending on the reporting date",
    "Admissions", "Probation, Supervised Release, Parole, BOP Custody", "E-1: Received for post-conviction supervision, by type", "People received, by type, excluding transfers in", "12 months ending on the reporting date",
    "Exits", "Total Supervision", "E-1: Removed from post-conviction supervision, total removed", "All people removed, including transfers out to other districts", "12 months ending on the reporting date",
    "Exits", "Probation, Supervised Release, Parole, BOP Custody", "E-1: Removed from post-conviction supervision, by type", "People removed, by type, excluding transfers out", "12 months ending on the reporting date",
    "Per 10,000 (any measure above)", "Same as the measure", "Calculated by the dashboard", "The count divided by the resident population of the place and date, times 10,000", "Same as the underlying measure"
  )

ab_dict_download <-
  tribble(
    ~Column, ~Meaning,
    "Year and Quarter", "Reporting date label, such as 2025, Q4 (the quarter ending December 31, 2025).",
    "State Name, State District or Circuit", "The place. Which column appears depends on the Geography Level you choose.",
    "Supervision Type", "Total Supervision, Supervised Release, Probation, Parole or BOP Custody.",
    "Admissions", "People received in the 12 months ending on the date. For Total Supervision this includes transfers in.",
    "Exits", "People removed in the 12 months ending on the date. For Total Supervision this includes transfers out.",
    "Total Population", "People under supervision on the date.",
    "Admissions Per 10,000, Exits Per 10,000, Total Population Per 10,000", "The same measures per 10,000 residents. These replace the count columns when Per Capita is selected."
  )

ab_dict_fields <-
  tribble(
    ~Field, ~Meaning, ~Example,
    "circuit", "Circuit the row belongs to (1st to 11th, or DC). It is Total on national-total rows.", "9th",
    "state_or_district", "Geography code as reported: a district, a single-district state or territory, a circuit, or Total.", "TX,N",
    "state_name", "State or territory name. Blank for circuit and national-total rows.", "Texas",
    "state_district", "Judicial district name, built from the state and the division.", "Texas Northern",
    "table_num", "Source table: E-1, E-2, E-3, E-8, E-8A or E-10.", "E-2",
    "table_desc", "Description of the table for that release.", "Persons Under Post-Conviction as of December 31, 2025",
    "table_name_full", "Full table title as published.", "Table E-2. Federal Probation System ...",
    "table_name_date", "Date taken from the table's file name.", "2025-12-31",
    "file_name", "Identifier of the source file.", "e-2_2025-12-31",
    "file_type", "Format of the source file (PDF or XLSX).", "XLSX",
    "date", "Date printed inside the Excel file. Blank for PDFs.", "(blank)",
    "date_full", "Reporting date used by the dashboards.", "2025-12-31",
    "year", "Calendar year of the reporting date.", "2025",
    "date_month", "Month number of the reporting date (3, 6, 9 or 12).", "12",
    "date_quarter", "Calendar quarter number (1 to 4).", "4",
    "quarter_name_full", "Year and quarter label used by the sliders.", "2025, Q4",
    "categories", "Section of the table the line belongs to.", "Received For Post-Conviction Supervision",
    "original_categories", "Label as it appeared in the source table, before labels were harmonized across years.", "received_for_supervision_judge_probation",
    "variables_simple", "Short name of the supervision type or line.", "probation",
    "variables", "Full variable name (section plus line). The dashboards use it to define each measure.", "from_courts_probation",
    "values", "Reported number of people.", "292",
    "population", "Resident population estimate for the district and year. Later dates reuse the most recent estimate.", "740,133",
    "values_per_capita", "Per-capita rate: 10,000 times values divided by population.", "3.95",
    "notes", "Footnotes printed on the table.", "Includes conditional release, probation, ..."
  )

ab_dict_other <-
  tribble(
    ~Table, ~Contents, ~`Coverage in the data`,
    "E-3", "Persons under post-conviction supervision, by offense (as of a date).", ov_other_cov("E-3"),
    "E-8", "Post-conviction supervision cases revoked and closed during the 12 months ending on a date.", ov_other_cov("E-8"),
    "E-8A", "Post-conviction supervision cases closed during the 12 months ending on a date.", ov_other_cov("E-8A"),
    "E-10", "Presentence investigations during the 12 months ending on a date.", ov_other_cov("E-10")
  )

ab_dictionary <-
  tagList(
    h3("What the dashboards measure"),
    p(class = "ab-lead", "Every number in the dashboards is built from one of these lines of Tables E-1 and E-2."),
    ab_table(ab_dict_measures),
    
    h3("Columns in the downloadable table"),
    p(class = "ab-lead", HTML("These are the columns you can copy or export from Main Dashboard &rsaquo; Table Filters.")),
    ab_table(ab_dict_download),
    
    h3("Fields in the underlying dataset"),
    p(class = "ab-lead", "The data behind the dashboards, one row per place, reporting date and table line. The repository's prepared file is data/federal_courts_df_final.rds."),
    ab_table(ab_dict_fields, code_cols = "Field"),
    
    h3("Other tables in the data"),
    p(class = "ab-lead", "These tables are collected and stored with the data but are not charted in the dashboards yet."),
    ab_table(ab_dict_other)
  )

##Sources and caveats
ab_sources <-
  tagList(
    h3("Where the data come from"),
    tags$ul(
      tags$li(HTML(paste0("<strong>Supervision counts:</strong> the federal judiciary's Federal Probation System tables E-1, E-2, E-3, E-8, E-8A and E-10, published in the ",
                          ab_link_html("https://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables", "Caseload Statistics Data Tables"),
                          " on uscourts.gov. Tables through ", fmt_month_year(ov_pdf_last), " were published as PDFs and later ones as Excel files. All were extracted by script."))),
      tags$li(HTML(paste0("<strong>Population:</strong> ",
                          ab_link_html("https://www.census.gov/programs-surveys/popest.html", "U.S. Census Bureau population estimates"),
                          " for counties, combined into judicial districts with a county-to-district crosswalk."))),
      tags$li(HTML(paste0("<strong>Code and data preparation:</strong> ",
                          ab_link_html("https://github.com/REFORM-Alliance/us-federal-court-data-dashboard", "the project's GitHub repository"), ".")))
    ),
    
    h3("Things to keep in mind"),
    tags$ol(class = "ab-caveats",
            tags$li(HTML("<strong>Snapshots and flows differ.</strong> Population is a headcount on one date. Admissions and exits are totals for the 12 months ending on that date, so consecutive quarters overlap. The note in the dashboard sidebars about the 12-month reporting period applies to admissions and exits.")),
            tags$li(HTML("<strong>Transfers are included.</strong> Admissions and exits count transfers between districts. For Admissions and Exits, Total Supervision is larger than the sum of the four supervision types because it includes transfers.")),
            tags$li(HTML(paste0("<strong>Some dates are excluded.</strong> When the sum of district figures differed from the national total on the same table by more than 5%, that line was dropped for that date. This is why admissions and exits are blank for ",
                                fmt_list(ov_flow_missing), "."))),
            tags$li(HTML("<strong>Reporting frequency changes.</strong> Data are annual (September 30) from 1997 to 2000 and quarterly from 2001, so early trend lines have fewer points.")),
            tags$li(HTML(paste0("<strong>Population denominators.</strong> Rates use total resident population of all ages, not just adults.",
                                ifelse(ov_pop_last_year < ov_year_latest,
                                       paste0(" The latest population estimate is for ", ov_pop_last_year, ", and later reporting dates reuse it."),
                                       "")))),
            tags$li(HTML(paste0("<strong>Small places can swing.</strong> Rates for small populations, including several territories, can change sharply with only a few cases. Territories are included in the State level (",
                                paste(ov_territories, collapse = ", "), "). American Samoa is not in the data."))),
            tags$li(HTML("<strong>These are counts of people, not measures of crime or performance.</strong> Differences between places reflect many factors, including enforcement priorities, the mix of federal cases and local population size."))
    )
  )

about_ui <-
  div(class = "ab-page",
      div(class = "ab-hero",
          h1("About this dashboard"),
          p(class = "ab-lede",
            "Explore federal post-conviction supervision data from the U.S. courts. Use the sections below to find the right view for your question, ",
            "look up unfamiliar terms, and see exactly what each number measures.")),
      tabsetPanel(
        id = "about_tabs",
        type = "pills",
        tabPanel("Ways to use it", div(class = "ab-section", ab_use_cards)),
        tabPanel("Key terms", div(class = "ab-section", ab_glossary)),
        tabPanel("Data dictionary", div(class = "ab-section", ab_dictionary)),
        tabPanel("Sources and caveats", div(class = "ab-section", ab_sources))
      )
  )


##Styles and Scripts for the Overview and About Pages
overview_about_css <- "
      /* --- OVERVIEW & ABOUT PAGES --- */
      .ov-page, .ab-page {
        max-width: 1180px;
        margin: 0 auto;
        padding: 24px 20px 64px;
        color: #2C2C2C;
      }
      .ov-page a:not(.ov-card-link), .ab-page a:not(.nav-link) {
        color: #00785A;
        text-decoration: underline;
      }
      .ov-hero h1, .ab-hero h1 {
        font-size: 2rem;
        font-weight: 700;
        line-height: 1.2;
        margin: 0 0 14px;
      }
      .ov-lede, .ab-lede {
        font-size: 1.15rem;
        line-height: 1.55;
        max-width: 860px;
        margin-bottom: 12px;
      }
      .ov-body {
        font-size: 1rem;
        line-height: 1.6;
        max-width: 860px;
        color: #3b3b3b;
        margin-bottom: 12px;
      }
      .ov-asof {
        display: inline-block;
        font-size: 0.85rem;
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 999px;
        padding: 4px 12px;
      }

      /* Headline numbers */
      .ov-kpis {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        gap: 16px;
        margin: 28px 0 8px;
      }
      .ov-kpi {
        background: #fff;
        border: 1px solid #E6DCC3;
        border-top: 4px solid #009E73;
        border-radius: 8px;
        padding: 14px 16px;
      }
      .ov-kpi-label {
        font-size: 0.78rem;
        font-weight: 500;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        color: #5c5c5c;
      }
      .ov-kpi-value {
        font-size: 2.1rem;
        font-weight: 700;
        line-height: 1.15;
        color: #00785A;
        margin: 2px 0;
      }
      .ov-kpi-note {
        font-size: 0.85rem;
        line-height: 1.4;
        color: #555;
      }

      /* Trend blocks */
      .ov-section-title {
        font-size: 1.5rem;
        font-weight: 700;
        margin: 40px 0 6px;
      }
      .ov-section-sub {
        color: #555;
        margin-bottom: 16px;
      }
      .ov-trend {
        display: grid;
        grid-template-columns: minmax(0, 5fr) minmax(0, 7fr);
        gap: 28px;
        align-items: start;
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 10px;
        padding: 22px;
        margin-bottom: 20px;
      }
      .ov-trend h3 {
        font-size: 1.2rem;
        font-weight: 700;
        line-height: 1.3;
        margin: 0 0 8px;
      }
      .ov-trend h3:not(:first-child) {
        margin-top: 22px;
      }
      .ov-trend p {
        line-height: 1.6;
        margin-bottom: 10px;
      }
      .ov-trend-chart {
        min-width: 0;
      }
      .ov-caption {
        font-size: 0.78rem;
        line-height: 1.45;
        color: #666;
        margin-top: 6px;
      }

      /* Links to the dashboards */
      .ov-explore {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
        gap: 16px;
        margin-top: 12px;
      }
      .ov-card-link {
        display: block;
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 8px;
        padding: 16px;
        text-decoration: none !important;
        color: #2C2C2C !important;
        transition: border-color 0.15s, box-shadow 0.15s;
      }
      .ov-card-link:hover, .ov-card-link:focus {
        border-color: #009E73;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.08);
      }
      .ov-card-link .t {
        display: block;
        font-weight: 700;
        color: #00785A;
        margin-bottom: 4px;
      }
      .ov-card-link .d {
        display: block;
        font-size: 0.9rem;
        line-height: 1.45;
      }
      .ov-notes {
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 10px;
        padding: 16px 22px;
        margin-top: 32px;
      }
      .ov-notes h3 {
        font-size: 1.05rem;
        font-weight: 700;
        margin: 0 0 8px;
      }
      .ov-notes ul {
        padding-left: 1.2rem;
        margin-bottom: 8px;
      }
      .ov-notes li {
        line-height: 1.55;
        margin-bottom: 4px;
      }

      /* About page */
      .ab-page .nav-pills {
        margin-bottom: 8px;
      }
      .ab-page .nav-pills .nav-link {
        border-radius: 999px;
        font-weight: 500;
        color: #00785A;
      }
      .ab-page .nav-pills .nav-link.active {
        background-color: #00785A;
        color: #fff;
      }
      .ab-section {
        padding-top: 14px;
      }
      .ab-section h3 {
        font-size: 1.15rem;
        font-weight: 700;
        margin: 28px 0 8px;
      }
      .ab-section h3:first-child {
        margin-top: 0;
      }
      .ab-lead {
        color: #555;
        max-width: 860px;
      }
      .ab-card-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(340px, 1fr));
        gap: 18px;
      }
      .ab-card {
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 10px;
        padding: 18px 18px 14px;
      }
      .ab-card h3 {
        font-size: 1.1rem;
        margin: 0 0 8px;
      }
      .ab-where {
        display: inline-block;
        font-size: 0.8rem;
        font-weight: 500;
        background: #E6F4EF;
        color: #00583F;
        border-radius: 999px;
        padding: 3px 10px;
        margin-bottom: 10px;
      }
      .ab-card ol {
        padding-left: 1.2rem;
        margin-bottom: 10px;
      }
      .ab-card li {
        line-height: 1.5;
        margin-bottom: 6px;
      }
      .ab-tip, .ab-caution {
        font-size: 0.9rem;
        line-height: 1.5;
        padding: 8px 10px;
        border-radius: 6px;
        margin-top: 8px;
      }
      .ab-tip {
        background: #F1F8F5;
        border-left: 4px solid #009E73;
      }
      .ab-caution {
        background: #FFF3DF;
        border-left: 4px solid #D55E00;
      }
      .ab-glossary-search {
        max-width: 520px;
        margin-bottom: 8px;
      }
      .ab-term-group h3 {
        padding-bottom: 6px;
        border-bottom: 2px solid #E6DCC3;
      }
      .ab-term {
        margin: 0;
        padding: 10px 0;
        border-bottom: 1px solid #EFE6CF;
      }
      .ab-term dt {
        font-weight: 700;
      }
      .ab-term dd {
        margin: 2px 0 0;
        line-height: 1.55;
        max-width: 860px;
      }
      .ab-table-wrap {
        overflow-x: auto;
        background: #fff;
        border: 1px solid #E6DCC3;
        border-radius: 8px;
        margin-bottom: 8px;
      }
      .ab-table {
        margin: 0;
        font-size: 0.9rem;
      }
      .ab-table th {
        background: #F6EED8;
        font-weight: 700;
        white-space: nowrap;
      }
      .ab-table td {
        vertical-align: top;
      }
      .ab-table code {
        color: #8A3B00;
        background: transparent;
        padding: 0;
      }
      .ab-caveats li, .ab-section ul li {
        line-height: 1.6;
        margin-bottom: 8px;
        max-width: 860px;
      }

      @media (max-width: 860px) {
        .ov-trend { grid-template-columns: 1fr; }
      }
      @media (max-width: 576px) {
        .ov-hero h1, .ab-hero h1 { font-size: 1.6rem; }
        .ab-card-grid { grid-template-columns: 1fr; }
      }
"

overview_about_js <- "
  // Jump back to the top when switching between the main tabs
  $(document).on('shown.bs.tab', '.navbar a[data-toggle=tab]', function(){
    window.scrollTo(0, 0);
  });
  // Filter the key terms list as the user types
  $(document).on('input', '#glossary-filter', function(){
    var q = $(this).val().toLowerCase().trim();
    $('.ab-term-group').each(function(){
      var anyShown = false;
      $(this).find('.ab-term').each(function(){
        var show = (q === '' || $(this).text().toLowerCase().indexOf(q) !== -1);
        $(this).toggle(show);
        if(show) anyShown = true;
      });
      $(this).toggle(anyShown);
    });
  });
"


####App####
##UI
ui <- navbarPage("US Federal Court Supervision Data Dashboard",
                 id = "main_nav",
                 selected = "Overview",
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
    ")),
                   tags$style(HTML(overview_about_css)),
                   tags$script(HTML(overview_about_js))
                 ),
                 
                 tabPanel("Overview", overview_ui),
                 
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
                                            plotlyOutput("national_viz_trends", height = 800, width = "100%")))
                                  ),
                                  fluidRow(
                                    box(width = 12,
                                        div(style = "width:100%; padding:0; margin:0; text-align:right;",
                                            plotlyOutput("national_viz_stacked_barplot", height = 600, width = "100%")))
                                  )
                                )
                              )
                          )
                 ),
                 
                 tabPanel("About", about_ui)
)

##Server
server <- function(input, output, session){
  
  # CHANGE 1: Render year selector UI for map/barplot filters (toggle-based)
  output$year_selector_map <- renderUI({
    if(input$year_mode_map == "single"){
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
    if(input$year_mode_table == "single"){
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
    if(input$year_mode_map == "single") "" else paste0(str_to_title(input$agg_level), " ")
  })
  
  # Helper: get selected quarters for map/barplot
  map_selected_quarters <- reactive({
    if(input$year_mode_map == "single"){
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
  #   if(input$year_mode_table == "single"){
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
    if(input$year_mode_table == "single"){
      req(input$single_year_table_val, input$single_quarter_table_val)
      target <- paste0(input$single_year_table_val, ", ", input$single_quarter_table_val)
      quarters_full[quarters_full == target]
    } else {
      yr <- input$year_range_table
      if(is.null(yr)) return(quarters_full)  # default to full range on load
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
          add_basemap() %>% 
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
          add_basemap() %>% 
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
          add_basemap() %>% 
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
          add_basemap() %>% 
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
          add_basemap() %>% 
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
          add_basemap() %>% 
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
      filter(is.na(state_name) == FALSE) %>% 
      {
        base_data <- .
        bind_rows(
          base_data,
          base_data %>%  
            group_by(date, supervision_type, outcome_metric_type, quarter_name_full) %>%
            dplyr::summarize(across(c("values", "values_per_capita"), 
                                    ~.x %>% 
                                      mean(na.rm = TRUE))) %>% 
            ungroup() %>% 
            mutate(state_name = "National Mean"),
          base_data %>%
            group_by(date, supervision_type, outcome_metric_type, quarter_name_full) %>%
            dplyr::summarize(
              values_per_capita = weightedMedian(values_per_capita, w = population, na.rm = TRUE),
              values            = weightedMedian(values, w = population, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(state_name = "National Median")
        )
      } %>% 
      filter(state_name %in% c(input$state_selection, "National Mean", "National Median") & 
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
          scale_y_continuous(labels = scales::comma) +
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
      margin = list(l = 95, r = 180, t = 40, b = 55))
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
      mutate(values_per_capita = 
               10000 * (values / population),
             plot_val = if(use_per_capita) values_per_capita else values) %>%
      filter(state_name == input$state_selection & quarter_name_full %in% selected_quarters) %>% 
      group_by(supervision_type, outcome_metric_type, state_name) %>% 
      dplyr::summarize(plot_val = 
                         plot_val %>%
                         mean(na.rm = TRUE)) %>% 
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
    ylab <- 
      ifelse(input$rate_or_count_national == "Per Capita", 
             "National Count per 10,000",  
             "National Total Count") 
    
    df <- 
      national_base_data() %>%
      filter(as.character(supervision_type) != "Total Supervision") %>%
      mutate(
        hover_text = paste0(
          "Supervision: ", supervision_type, "\n",
          "Outcome: ", outcome_metric_type, "\n",
          "National Total: ", format(values, big.mark = ","), "\n",
          "Per 10k: ", round(values_per_capita, 2), "\n",
          "Date: ", format(date, "%b %d, %Y")
        )
      ) 
    
    # df_total_pop <- 
    #   df %>% 
    #   filter(outcome_metric_type == "Total Population")
    # 
    # df_exits_admissions <- 
    #   df %>% 
    #   filter(outcome_metric_type != "Total Population")
    # 
    # base_plot <- function(data){
    #   data %>% 
    #     ggplot(
    #       aes(x = date,
    #           y = if(input$rate_or_count_national == "Per Capita") values_per_capita else values,
    #           color = supervision_type,
    #           group = supervision_type,
    #           text = hover_text)) +
    #     geom_line(linewidth = 1) +
    #     geom_point(size = 1) +
    #     scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73", "#CC79A7")) +
    #     scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    #     scale_y_continuous(labels = scales::comma) +
    #     theme_minimal()
    # }
    # 
    # p_total_pop <- 
    #   df_total_pop %>% 
    #   base_plot() + 
    #   facet_wrap(~outcome_metric_type, nrow = 1, scales = "free_y")
    # 
    # p_exits_admissions <- 
    #   df_exits_admissions %>% 
    #   base_plot() + 
    #   facet_wrap(~outcome_metric_type, nrow = 1, scales = "free_y")
    # 
    # p_total_pop_plotly <- 
    #   p_total_pop %>% 
    #   ggplotly(tooltip = "text") 
    # 
    # p_total_pop_plotly$x$data <- lapply(
    #   p_total_pop_plotly$x$data,
    #   function(tr){ tr$showlegend <- FALSE; tr }
    # )
    # 
    # p_exits_admissions_plotly <- 
    #   p_exits_admissions %>% 
    #   ggplotly(tooltip = "text") %>%
    #   layout(showlegend = TRUE)
    
    # subplot(
    #   p_total_pop_plotly,
    #   p_exits_admissions_plotly,
    #   nrows = 2,
    #   heights = c(0.65, 0.35),
    #   shareX = TRUE,
    #   titleY = TRUE, 
    #   margin = 0.1
    # ) %>%
    #   layout(
    #     margin = list(l = 120, r = 50, t = 60, b = 80),
    #     
    #     xaxis2 = list(title = NULL),
    #     yaxis = list(title = NULL),
    #     
    #     legend = list(
    #       title = list(text = "Supervision Type"),
    #       orientation = "h",
    #       x = 0.5,
    #       xanchor = "center",
    #       y = -0.15
    #     ),
    #     
    #     title = list(
    #       text = "National Supervision Trends Over Time",
    #       x = 0.52
    #     ),
    #     
    #     annotations = list(
    #       list(
    #         text = ylab,
    #         x = -0.04,
    #         y = 0.5,
    #         xref = "paper",
    #         yref = "paper",
    #         textangle = -90,
    #         showarrow = FALSE,
    #         xanchor = "center",
    #         yanchor = "middle"
    #       ),
    #       list(
    #         text = "Date",
    #         x = 0.5,
    #         y = -0.075,
    #         xref = "paper",
    #         yref = "paper",
    #         showarrow = FALSE,
    #         xanchor = "center",
    #         yanchor = "top"
    #       )
    #     )
    #   )
    
    # combined <- p_total_pop / p_exits_admissions + plot_layout(heights = c(2, 1))
    # 
    # combined %>% 
    #   ggplotly(tooltip = "text")
    
    
    df %>%
      mutate(
        outcome_metric_type = 
          outcome_metric_type %>% 
          factor(levels = c("Total Population", 
                            setdiff(unique(outcome_metric_type), "Total Population")))) %>% 
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
            y = ylab,
            color = "Supervision Type"
            # ,
            # title = "National Federal Supervision Trends Over Time"
          ) +
          theme_minimal() +
          theme(
            axis.text.x  = element_text(angle = 45, hjust = 1),
            strip.text   = element_text(size = 12, face = "bold"),
            strip.background = element_blank(),
            strip.switch.pad.grid = unit(0.3, "cm"), 
            legend.position = "bottom",
            axis.title.x = element_text(margin = margin(t = 15)),
            plot.title   = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 25)), 
            plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
          )
      } %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15, yanchor = "top"),
        margin = list(l = 50, r = 50, t = 70, b = 55)
      )
  })
  
  # ---- Overview page outputs ----
  output$ov_pop_chart  <- renderPlotly({ ov_plot_pop() })
  output$ov_flow_chart <- renderPlotly({ ov_plot_flow() })
  output$ov_geo_chart  <- renderPlotly({ ov_plot_geo() })
  
  # Links from the Overview page to the other tabs
  observeEvent(input$ov_go_main,     { updateNavbarPage(session, "main_nav", selected = "Main Dashboard") })
  observeEvent(input$ov_go_state,    { updateNavbarPage(session, "main_nav", selected = "State Dashboard") })
  observeEvent(input$ov_go_national, { updateNavbarPage(session, "main_nav", selected = "National Dashboard") })
  observeEvent(input$ov_go_about,    { updateNavbarPage(session, "main_nav", selected = "About") })
}

####Run App####
shinyApp(ui, server)