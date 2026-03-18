rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(DBI)
library(dbplyr)
library(RPostgres)
library(RSelenium)
library(curl)
library(glue)
library(rvest)
library(readxl)
library(zoo)
library(pdftools)
library(sf)


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


####Parse Data####
##XLSX
xlsx_data <- 
  "data-raw" %>% 
  here("usaoc-data-tables") %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("xlsx$") %>% 
  str_subset("e-7", negate = TRUE) %>% 
  sort() %>% 
  map(~.x %>% 
        read_excel(col_names = FALSE) %>%
        mutate(file_name = 
                 .x %>% 
                 basename() %>% 
                 str_replace_all("[.]xlsx$", ""),
               across(where(is_character), 
                      ~.x %>% 
                        str_replace_all("\r\n", " ") %>% 
                        str_replace_all("\n", " ") %>% 
                        str_replace_all("  ", " "))) %>% 
        clean_names() %>% 
        select(where(~ !all(is.na(.)))) %>% 
        mutate(table_name = 
                 ifelse(str_detect(x1, "Table E-") == TRUE, x1, NA)) %>% 
        tidyr::fill(table_name, .direction = "down") %>% 
        mutate(x1 = 
                 x1 %>% 
                 replace_na("NA")) %>% 
        filter(!(str_detect(x1, "Table E-") == TRUE)) %>% 
        mutate(x1 = 
                 x1 %>% 
                 na_if("NA"),
               row_header_flag = 
                 ifelse(x1 == "Circuit and District", 1, NA)) %>% 
        tidyr::fill(row_header_flag, .direction = "down") %>%
        group_by(row_header_flag) %>% 
        mutate(row_header_flag = 
                 row_header_flag %>% 
                 cumsum()) %>% 
        ungroup() %>% 
        rownames_to_column("row_num") %>% 
        mutate(row_num = 
                 row_num %>% 
                 as.numeric()) %>% 
        filter(!if_all(starts_with("x"), is.na)) %>% 
        mutate(row_header_flag = 
                 ifelse(x1 == "TOTAL", 1, NA)) %>% 
        tidyr::fill(row_header_flag, .direction = "down") %>% 
        mutate(row_header_flag = 
                 row_header_flag %>% 
                 replace_na(0) %>% 
                 cumsum()) %>% 
        group_by(across(-c(x1, row_num))) %>%
        dplyr::summarize(x1 =
                           x1 %>%
                           paste(collapse = " "),
                         row_num =
                           row_num %>%
                           min(na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(x1 =
                 x1 %>%
                 na_if("NA")) %>%
        arrange(row_num) %>%
        mutate(table_desc =
                 ifelse(row_num == 1, x1, NA), 
               table_desc_new =
                 ifelse(row_num == 1 & table_desc == "Circuit and District", NA, table_desc),
               table_desc = table_desc_new) %>% 
        dplyr::select(-table_desc_new) %>%
        tidyr::fill(table_desc, .direction = "down") %>%
        mutate(row_num_new = case_when(row_num == 1 & x1 == "Circuit and District" & is.na(table_desc) == TRUE ~ row_num, 
                                       row_num == 1 & is.na(table_desc) == FALSE ~ NA, 
                                       row_num > 1 ~ row_num, 
                                       TRUE ~ NA),
               row_num = row_num_new) %>% 
        dplyr::select(-row_num_new) %>% 
        filter(is.na(row_num) == FALSE) %>%
        mutate(row_header_flag =
                 ifelse(x1 == "TOTAL", 1, NA)) %>%
        tidyr::fill(row_header_flag, .direction = "down") %>%
        group_by(row_header_flag) %>%
        mutate(row_header_flag =
                 row_header_flag %>%
                 cumsum()) %>%
        ungroup() %>%
        mutate(row_header_flag =
                 row_header_flag %>%
                 replace_na(0)) %>%
        group_by(row_header_flag) %>%
        mutate(row_num = ifelse(row_header_flag == 0, row_num - min(row_num, na.rm = TRUE) + 1, row_num)) %>%
        ungroup() %>%
        relocate(x1, .before = "x2") %>%
        mutate(table_desc = 
                 table_desc %>% 
                 replace_na("Missing")) %>% 
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var = "row_names") %>%
        mutate(row_names_new =
                 row_names %>%
                 str_extract_all("^x")) %>%
        group_by(row_names_new) %>%
        tidyr::fill(V1, .direction = "down") %>%
        ungroup() %>%
        dplyr::select(-row_names_new) %>%
        t() %>%
        as.data.frame() %>%
        row_to_names(row_number = 1) %>%
        group_by(row_header_flag) %>%
        dplyr::summarize(across(starts_with("x"),
                                ~.x %>%
                                  na.omit() %>%
                                  paste(collapse = " ")),
                         across(c(!starts_with("x"), -row_num),
                                ~.x %>%
                                  unique()),
                         row_num =
                           row_num %>%
                           min(na.rm = TRUE)) %>%
        ungroup() %>%
        dplyr::select(-row_num) %>%
        mutate(across(where(is_character),
                      ~.x %>%
                        na_if("")),
               row_header_flag =
                 row_header_flag %>%
                 as.numeric(),
               across(c("file_name", "table_name", "table_desc"),
                      ~ifelse(row_header_flag == 0, cur_column(), .x))) %>%
        dplyr::select(-row_header_flag) %>%
        mutate(row_num = row_number(), 
               across(where(is_character), 
                      ~ifelse(row_num == 1, 
                              .x %>% 
                                str_trim(side = "both") %>% 
                                str_replace_all("[\u00B9-\u00B9\u00B2-\u00B3\u2070-\u2079]$", "") %>% 
                                str_remove("(?<=\\D)\\d$"),
                              .x))) %>% 
        dplyr::select(-row_num) %>% 
        row_to_names(row_number = 1) %>%
        clean_names() %>%
        select(where(~ !all(is.na(.)))) %>% 
        mutate(state_or_district =
                 ifelse(is.na(circuit_and_district) == TRUE, circuit_and_district_2, circuit_and_district)) %>%
        dplyr::select(-contains("circuit_and_district_2")) %>%
        filter(is.na(state_or_district) == FALSE) %>%
        mutate(circuit_and_district = 
                 ifelse(str_detect(circuit_and_district, "^[:digit:]") == TRUE, circuit_and_district, NA), 
               notes_flag =
                 ifelse(str_detect(state_or_district, "^[:digit:] |^[\u00B9-\u00B9\u00B2-\u00B3\u2070-\u2079] ") == TRUE, 0, 1),
               circuit_and_district =
                 ifelse(notes_flag == 0 & is.na(circuit_and_district) == TRUE, state_or_district, circuit_and_district),
               state_or_district = 
                 ifelse(notes_flag == 0, NA, state_or_district),
               notes_flag_lag =
                 notes_flag %>%
                 lag(1) %>%
                 replace_na(0),
               notes_flag_sum =
                 notes_flag + notes_flag_lag,
               notes_flag_max =
                 ifelse(notes_flag_sum > 1, 1, notes_flag_sum),
               notes_flag_cumsum =
                 notes_flag_max %>%
                 cumsum()) %>%
        tidyr::fill(circuit_and_district, .direction = "down") %>%
        relocate(state_or_district, .after = "circuit_and_district") %>%
        dplyr::select(-c("notes_flag_lag", "notes_flag_sum", "notes_flag_max")) %>%
        mutate(notes_flag =
                 ifelse(notes_flag == 0, 1, 0)) %>%
        group_by(across(-c(circuit_and_district))) %>%
        dplyr::summarize(across(c(circuit_and_district),
                                ~.x %>% 
                                  paste(collapse = "; ")),
                         notes_flag =
                           notes_flag %>%
                           max(na.rm = TRUE)) %>%
        ungroup() %>%
        relocate(circuit_and_district, .before = 1) %>%
        arrange(notes_flag_cumsum) %>%
        mutate(notes =
                 ifelse(notes_flag == 1, circuit_and_district, NA)) %>%
        group_by(file_name, table_name, table_desc) %>%
        tidyr::fill(notes, .direction = "updown") %>%
        ungroup() %>%
        mutate(across(contains("supervision"),
                      ~.x %>%
                        as.numeric())) %>% 
        filter(is.na(state_or_district) == FALSE) %>% 
        dplyr::select(-c(notes_flag, notes_flag_cumsum)) %>% 
        distinct() %>% 
        mutate(across(where(is_character), 
                      ~.x %>% 
                        na_if("NA") %>% 
                        na_if("Missing"))) %>% 
        rename_with(~.x %>% 
                      str_replace_all("^total$", "presentence_total")) %>% 
        mutate(across(-any_of(c("circuit_and_district", "state_or_district", 
                                "file_name", "table_name", "table_desc", "notes")), 
                      ~.x %>% 
                        as.numeric())) %>% 
        select(where(~ !all(is.na(.)))) %>% 
        pivot_longer(cols = -any_of(c("circuit_and_district", "state_or_district", 
                                      "file_name", "table_name", "table_desc", "notes")),
                     names_to = "categories",
                     values_to = "values") 
  ) %>% 
  bind_rows() %>% 
  relocate(c("file_name", "table_name", "table_desc"), .after = "state_or_district") %>% 
  relocate("notes", .after = last_col()) %>% 
  mutate(table_name_full = 
           ifelse(is.na(table_desc) == TRUE, table_name, paste0(table_name, table_desc, sep = " ")) %>% 
           str_replace_all(" -- ", "—") %>% 
           str_replace_all(" ---- ", "—") %>% 
           str_replace_all(" —", "—") %>% 
           str_replace_all("[.] ", ".") %>% 
           str_replace_all(" - ", "—") %>% 
           str_replace_all("— ", "—") %>% 
           str_trim(side = "both")) %>% 
  dplyr::select(-c(table_desc, table_name)) %>% 
  separate_wider_delim(table_name_full, names = c("table_num", "table_desc"), delim = "—", cols_remove = FALSE) %>%
  mutate(table_num = 
           table_num %>% 
           str_replace_all("^Table ", "") %>% 
           str_replace_all("[.]", "") %>% 
           str_replace_all("Federal Probation System", ""),
         date = 
           categories %>% 
           str_extract_all("[^_]+_[^_]+_[^_]+$") %>% 
           str_replace_all("_", " ") %>% 
           mdy() %>% 
           suppressWarnings(),
         original_categories = categories,
         categories = 
           ifelse(str_detect(categories, "persons_under_supervision_") == TRUE, "persons_under_supervision", categories)) %>% 
  separate_wider_delim(file_name, names = c("file_name_table_num", "file_name_table_desc"), delim = "_", cols_remove = FALSE, too_few = "align_start") %>% 
  mutate(table_num = 
           ifelse(table_num == "", str_to_upper(file_name_table_num), table_num) %>% 
           str_trim(side = "both"), 
         across(where(is_character), 
                ~.x %>% 
                  na_if(""))) %>% 
  rename("table_name_date" = "file_name_table_desc") %>% 
  dplyr::select(-file_name_table_num) 

##PDF
y_tolerance <- 7
x_tolerance <- 25
max_x <- 792
max_y <- 612

pdf_data <- 
  "data-raw" %>% 
  here("usaoc-data-tables") %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("pdf$") %>% 
  str_subset("e-7", negate = TRUE) %>% 
  str_subset("e-1|e-2|e-3") %>% 
  map(~.x %>% 
        pdf_data(font_info = TRUE) %>% 
        set_names(c(1:as.numeric(length(.)))) %>% 
        map(~.x %>% 
              mutate(table_name_flag = 
                       ifelse(font_size == max(font_size, na.rm = TRUE), 1, 0),
                     row_name_flag = 
                       ifelse(table_name_flag == 0, 1, 0) %>% 
                       cumsum(),
                     num_tabs = 
                       text %>% 
                       str_count("\t"), 
                     tab_width_adjusted = num_tabs * 46 * (font_size / 8),
                     width_new = 
                       width - tab_width_adjusted,
                     width = 
                       ifelse(width_new < 0, width, width_new)) %>% 
              dplyr::select(-c(num_tabs, tab_width_adjusted, width_new)) %>% 
              group_by(table_name_flag) %>% 
              mutate(margin_x_min = 
                       ifelse(table_name_flag == 1, min(x, na.rm = TRUE), NA), 
                     margin_y_min = 
                       ifelse(table_name_flag == 1, min(y, na.rm = TRUE), NA),
                     margin_x_max = max_x,
                     margin_y_max = max_y) %>% 
              ungroup() %>% 
              tidyr::fill(c("margin_x_min", "margin_y_min"), .direction = "downup") %>% 
              mutate(text_num = 
                       text %>% 
                       as.numeric() %>% 
                       suppressWarnings()) %>% 
              group_by(font_name) %>% 
              mutate(font_name_group_size = n()) %>% 
              ungroup() %>%
              mutate(margin_x_min_count = 
                       sum(x <= margin_x_min - 10, na.rm = TRUE),
                     margin_y_min_count = 
                       sum(y <= margin_y_min - 10, na.rm = TRUE)) %>% 
              filter(!(x <= (margin_x_min - 10) & margin_x_min_count <= 10)) %>%
              filter(!(y <= (margin_y_min - 10) & margin_y_min_count <= 10)) %>%
              filter(!(x == min(x, na.rm = TRUE) & is.na(text_num) == FALSE & font_name_group_size == 1)) %>% 
              filter(!(y == min(y, na.rm = TRUE) & is.na(text_num) == FALSE & font_name_group_size == 1)) %>% 
              filter(!(x == max(x, na.rm = TRUE) & is.na(text_num) == FALSE & font_name_group_size == 1)) %>% 
              filter(!(y == max(y, na.rm = TRUE) & is.na(text_num) == FALSE & font_name_group_size == 1)) %>% 
              dplyr::select(-c(text_num, font_name_group_size)) %>% 
              mutate(space_flag = 
                       ifelse(space == TRUE, 1, 0)) %>%
              group_by(row_name_flag, font_name) %>%
              dplyr::summarize(across(c(where(is.numeric), where(is.integer), -width), 
                                      ~.x %>% 
                                        max(na.rm = TRUE)),
                               width = 
                                 width %>% 
                                 sum(na.rm = TRUE),
                               text = 
                                 text %>% 
                                 paste(collapse = " ")) %>% 
              ungroup() %>% 
              mutate(table_name_full = 
                       ifelse(table_name_flag == 1, text, NA)) %>% 
              tidyr::fill(table_name_full, .direction = "down") %>% 
              filter(!(table_name_flag == 1)) %>% 
              dplyr::select(-table_name_flag) %>% 
              mutate(text = 
                       text %>% 
                       str_trim(side = "both") %>% 
                       na_if("")) %>% 
              filter(is.na(text) == FALSE) %>%  
              arrange(y) %>% 
              mutate(row_id = 
                       y %>% 
                       dist() %>% 
                       hclust(method = "centroid") %>% 
                       cutree(h = y_tolerance),
                     row_id_1 = 
                       ifelse(row_id == 1, 0, 1) %>% 
                       cumsum(), 
                     max_col_nums = 
                       row_id %>% 
                       table() %>% 
                       table() %>% 
                       which.max() %>% 
                       names() %>% 
                       as.numeric(),
                     max_col_nums = max_col_nums + 1) %>% 
              arrange(x) %>%
              mutate(col_id =
                       x %>%
                       dist() %>%
                       hclust(method = "centroid") %>%
                       cutree(k = max_col_nums) %>% 
                       diag(), 
                     top_row_header_col_id_e2 = 
                       x %>% 
                       dist() %>% 
                       hclust(method = "centroid") %>% 
                       cutree(k = 2) %>% 
                       diag() %>% 
                       diag(),
                     col_id = 
                       case_when(str_detect(table_name_full, "Table E-2") == TRUE & row_id_1 == 0 ~ top_row_header_col_id_e2,
                                 str_detect(table_name_full, "Table E-1") == TRUE & row_id_1 == 0 ~ 1,
                                 row_id_1 > 0 ~ col_id,
                                 TRUE ~ col_id)) %>%
              group_by(row_id_1, font_name, table_name_full, col_id) %>% 
              dplyr::summarize(across(c(where(is.numeric), where(is.integer), -width, -space_flag), 
                                      ~.x %>% 
                                        min(na.rm = TRUE)),
                               width = 
                                 width %>% 
                                 sum(na.rm = TRUE),
                               space_flag = 
                                 space_flag %>% 
                                 max(na.rm = TRUE),
                               text = 
                                 text %>% 
                                 paste(collapse = " ")) %>% 
              ungroup() %>% 
              mutate(text = 
                       text %>% 
                       str_trim(side = "both"))) %>%
        bind_rows(.id = "page_num") %>% 
        mutate(col_id = 
                 ifelse(col_id == 2, 1, col_id), 
               text = 
                 ifelse(col_id == 1, str_replace_all(text, "Total", "TOTAL"), text),
               first_table_row = 
                 ifelse(col_id == 1 & text == "TOTAL", row_id, NA)) %>% 
        group_by(page_num) %>% 
        mutate(first_table_row = 
                 ifelse(col_id == 1 & text == "Circuit", row_id + 1, NA)) %>% 
        tidyr::fill(first_table_row, .direction = "updown") %>% 
        mutate(first_table_row = 
                 first_table_row %>% 
                 max(na.rm = TRUE) %>% 
                 suppressWarnings()) %>% 
        ungroup() %>% 
        tidyr::fill(first_table_row, .direction = "updown") %>% 
        mutate(first_table_row = 
                 ifelse(is.infinite(first_table_row) == TRUE, 5, first_table_row)) %>% 
        mutate(header_flag = 
                 ifelse(row_id < first_table_row, 1, 0), 
               x_last = x + width,
               x_new = 
                 ifelse(header_flag == 1, x, x_last)) %>% 
        arrange(page_num, header_flag, x_new) %>%
        mutate(notes_flag_og = 
                 ifelse(font_size >= 3 & font_size <= 6.5 | text == "*", 1, 0) %>% 
                 as.numeric(),
               notes_flag_og = 
                 ifelse(page_num == max(page_num, na.rm = TRUE), notes_flag_og, 0),
               notes_flag_og = 
                 ifelse(header_flag == 1, 0, notes_flag_og)) %>% 
        arrange(page_num, row_id, x, y) %>% 
        group_by(page_num, row_id) %>% 
        mutate(notes_flag_og = 
                 notes_flag_og %>% 
                 max(na.rm = TRUE),
               notes_flag_og = 
                 notes_flag_og %>% 
                 cummax()) %>% 
        ungroup() %>% 
        arrange(page_num, desc(header_flag), x, row_id) %>% 
        group_by(page_num, header_flag, row_id) %>% 
        mutate(x_lead = 
                 x %>% 
                 lead(1)) %>% 
        ungroup() %>% 
        mutate(x_space_width = x_lead - x_last,
               space_flag = 
                 ifelse(x_space_width > 6, 0, space_flag), 
               space_flag = 
                 space_flag %>% 
                 replace_na(0),
               space_flag_result = 
                 ifelse(space_flag == 1, 0, 1)) %>% 
        group_by(page_num, header_flag, row_id) %>% 
        mutate(space_flag_result_lag = 
                 space_flag_result %>% 
                 lag(1), 
               space_flag_cumsum = 
                 space_flag_result_lag %>% 
                 replace_na(0) %>% 
                 cumsum()) %>% 
        ungroup() %>% 
        mutate(row_number_x = 
                 1:n(),
               row_number_x = 
                 ifelse(header_flag == 1, NA, row_number_x)) %>% 
        arrange(page_num, desc(header_flag), row_id, x) %>% 
        dplyr::select(-c(space_flag)) %>% 
        mutate(y_last = y + height) %>% 
        filter(!(font_size < 6 & header_flag == 1)) %>% 
        group_by(page_num, font_name, table_name_full, header_flag, notes_flag_og, row_id, space_flag_cumsum, first_table_row, max_col_nums, row_number_x) %>% 
        dplyr::summarize(across(c(width),
                                ~.x %>% 
                                  sum(na.rm = TRUE)),
                         across(c(starts_with("row_id"), starts_with("col_id"), 
                                  x, y, margin_x_min, margin_y_min, top_row_header_col_id_e2), 
                                ~.x %>% 
                                  min(na.rm = TRUE)),
                         across(c(x_last, y_last, margin_x_max, margin_y_max, margin_x_min_count, margin_y_min_count, font_size),
                                ~.x %>% 
                                  max(na.rm = TRUE)),
                         text = 
                           text %>% 
                           paste(collapse = " ")) %>% 
        ungroup() %>% 
        dplyr::select(-row_number_x) %>% 
        arrange(page_num, header_flag, notes_flag_og) %>%
        mutate(x_new = 
                 ifelse(header_flag == 1, (x + x_last)/2, x_last)) %>% 
        group_split(page_num, header_flag, notes_flag_og) %>%
        map(
          ~if(unique(.x$notes_flag_og) == 1){
            mutate(.x, col_id = NA)
          }else{
            mutate(.x, 
                   col_id =
                     case_when(notes_flag_og == 0 & header_flag == 1 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "centroid") %>%
                                 cutree(k = max_col_nums - 1) %>% 
                                 diag(),
                               notes_flag_og == 0 & header_flag == 0 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "centroid") %>%
                                 cutree(k = max_col_nums) %>% 
                                 diag(),
                               TRUE ~ NA))
          }) %>% 
        bind_rows() %>% 
        mutate(col_id = 
                 ifelse(header_flag == 0 & col_id == 2, 1, col_id),
               col_id = 
                 ifelse(header_flag == 1 & col_id > 1, col_id + 1, col_id)) %>% 
        group_by(page_num, header_flag, col_id) %>% 
        mutate(col_count = ifelse(header_flag == 0, n(), 99999)) %>% 
        ungroup() %>% 
        mutate(text_numeric = 
                 text %>% 
                 as.numeric() %>% 
                 suppressWarnings()) %>%
        filter(!(col_count == 1 & header_flag == 0 & notes_flag_og == 0 & is.na(text_numeric) == FALSE)) %>% 
        dplyr::select(-c(col_count, notes_flag_og, text_numeric)) %>% 
        arrange(page_num, header_flag, x) %>%
        mutate(notes_flag_og = 
                 ifelse(font_size >= 3 & font_size <= 6.5 | text == "*", 1, 0) %>% 
                 as.numeric(),
               notes_flag_og = 
                 ifelse(page_num == max(page_num, na.rm = TRUE), notes_flag_og, 0),
               notes_flag_og = 
                 ifelse(header_flag == 1, 0, notes_flag_og)) %>% 
        arrange(page_num, row_id, x, y) %>% 
        group_by(page_num, row_id) %>% 
        mutate(notes_flag_og = 
                 notes_flag_og %>% 
                 max(na.rm = TRUE),
               notes_flag_og = 
                 notes_flag_og %>% 
                 cummax()) %>% 
        ungroup() %>% 
        arrange(page_num, header_flag, notes_flag_og) %>%
        group_split(page_num, header_flag, notes_flag_og) %>%
        map(
          ~if(unique(.x$notes_flag_og) == 1){
            mutate(.x, col_id = NA)
          }else{
            mutate(.x, 
                   col_id =
                     case_when(notes_flag_og == 0 & header_flag == 1 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "centroid") %>%
                                 cutree(k = max_col_nums - 1) %>% 
                                 diag(),
                               notes_flag_og == 0 & header_flag == 0 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "centroid") %>%
                                 cutree(k = max_col_nums) %>% 
                                 diag(),
                               TRUE ~ NA))
          }) %>% 
        bind_rows() %>% 
        mutate(header2_x = 
                 ifelse(header_flag == 1 & col_id == 2, x, NA) %>% 
                 min(na.rm = TRUE)) %>% 
        group_by(page_num) %>% 
        tidyr::fill(header2_x, .direction = "updown") %>% 
        ungroup() %>% 
        mutate(x_new = 
                 ifelse(x <= header2_x, x, x_new)) %>% 
        arrange(page_num, header_flag, row_id, x_new) %>% 
        mutate(col_id_header = 
                 ifelse(header_flag == 1 & text %in% c("Circuit", "District"), 1, NA), 
               circuit = 
                 ifelse(text == "Circuit", text, NA),
               district = 
                 ifelse(text == "District", text, NA)) %>% 
        group_by(page_num) %>%  
        tidyr::fill(circuit, .direction = "down") %>% 
        tidyr::fill(district, .direction = "up") %>% 
        ungroup() %>% 
        group_by(page_num, header_flag, circuit, district) %>% 
        tidyr::fill(col_id_header, .direction = "updown") %>% 
        ungroup() %>% 
        dplyr::select(-c(circuit, district)) %>% 
        mutate(col_id_header = 
                 col_id_header %>% 
                 replace_na(0),
               col_id_header = 
                 ifelse(!(text %in% c("Circuit", "and", "District")), 0, col_id_header)) %>% 
        arrange(page_num, header_flag, notes_flag_og, col_id_header, x_new) %>% 
        group_split(page_num, header_flag, notes_flag_og, col_id_header) %>%
        map(
          ~if(unique(.x$notes_flag_og) == 1){
            mutate(.x, col_id = NA)
          }else if(unique(.x$col_id_header) == 1){
            mutate(.x, col_id = NA)
          }else{
            mutate(.x, 
                   col_id =
                     case_when(notes_flag_og == 0 & header_flag == 1 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "ward.D2") %>%
                                 cutree(k = max_col_nums - 1) %>% 
                                 diag(),
                               notes_flag_og == 0 & header_flag == 0 ~
                                 x_new %>%
                                 dist() %>%
                                 hclust(method = "ward.D2") %>%
                                 cutree(k = max_col_nums) %>% 
                                 diag(),
                               TRUE ~ NA))
          }) %>% 
        bind_rows() %>% 
        mutate(col_id = 
                 ifelse(header_flag == 1 & col_id >= 2, col_id + 1, col_id),
               col_id = 
                 ifelse(header_flag == 1 & col_id_header == 1, 1, col_id), 
               total_col_num = 
                 ifelse(x <= header2_x & 
                          (text == "TOTAL" | str_detect(text, regex("\\b\\d+(st|nd|rd|th)\\b", ignore_case = TRUE)) == TRUE) &
                          header_flag == 0,
                        col_id, 
                        NA),
               total_col_num_flag = total_col_num) %>% 
        dplyr::select(-col_id_header) %>% 
        group_by(page_num) %>% 
        tidyr::fill(total_col_num, .direction = "downup") %>% 
        ungroup() %>% 
        mutate(col_id_new = 
                 case_when(header_flag == 1 ~ col_id,
                           total_col_num == 1 & header_flag == 0 & col_id != max_col_nums & col_id == 1 & is.na(total_col_num_flag) == TRUE ~ col_id,
                           total_col_num == 1 & header_flag == 0 & col_id != max_col_nums & col_id == 1 & total_col_num_flag == 1 ~ col_id + 1,
                           total_col_num == 1 & header_flag == 0 & col_id != max_col_nums & col_id != 1 ~ col_id + 1, 
                           total_col_num == 0 ~ col_id,
                           TRUE ~ col_id),
               col_id = col_id_new) %>% 
        dplyr::select(-c(notes_flag_og, col_id_new, total_col_num, total_col_num_flag, header2_x)) %>% 
        mutate(col_id = 
                 ifelse(header_flag == 0 & col_id == 2, 1, col_id)) %>% 
        group_split(page_num) %>% 
        map(~.x %>% 
              arrange(desc(header_flag), row_id) %>% 
              mutate(header_flag2 = 
                       ifelse(header_flag == 1, 0, 1) %>% 
                       cumsum(), 
                     top_row_headers = 
                       ifelse(row_id == 1 & header_flag == 1, text, NA)) %>% 
              arrange(col_id) %>% 
              group_by(header_flag) %>% 
              tidyr::fill(top_row_headers, .direction = "downup") %>% 
              ungroup() %>% 
              filter(!(row_id == 1 & header_flag == 1)) %>% 
              filter(!(header_flag == 1 & font_size >= 3 & font_size <= 6.5)) %>%
              group_by(font_name, table_name_full, page_num, header_flag2, col_id, top_row_headers) %>% 
              dplyr::summarize(across(c(where(is.numeric), where(is.integer), -width), 
                                      ~.x %>% 
                                        min(na.rm = TRUE)),
                               width = 
                                 width %>% 
                                 sum(na.rm = TRUE),
                               text = 
                                 text %>% 
                                 paste(collapse = " ")) %>% 
              ungroup() %>% 
              mutate(top_row_headers = 
                       case_when(header_flag == 0 ~ NA, 
                                 header_flag == 1 & text == "Circuit and District" ~ NA, 
                                 header_flag == 1 & str_detect(text, "Supervision") == TRUE ~ NA,
                                 str_detect(table_name_full, "^Table E-1") == TRUE & header_flag == 1 ~ top_row_headers, 
                                 str_detect(table_name_full, "^Table E-3") == TRUE & 
                                   str_detect(text, "Homicide|Robbery|Assault|Burglary|Larceny|Embezzlement|Fraud|Auto Theft|Forgery and Counterfeiting|Drug Laws|Weapons and Firearms|Traffic|Immigration Laws") == TRUE & 
                                   header_flag == 1 ~ "General Offenses", 
                                 str_detect(table_name_full, "^Table E-3") == TRUE & 
                                   str_detect(text, "Liquor, Internal Revenue|Postal Laws") == TRUE & 
                                   header_flag == 1 ~ "Special Offenses", 
                                 str_detect(table_name_full, "^Table E-3") == TRUE & 
                                   str_detect(text, "Other") == TRUE & 
                                   header_flag == 1 ~ top_row_headers, 
                                 header_flag == 1 & text == "Total" & str_detect(table_name_full, "^Table E-3") == TRUE ~ NA,
                                 str_detect(text, "Probation") == TRUE & header_flag == 1 & str_detect(table_name_full, "^Table E-2") == TRUE ~ "From Courts", 
                                 str_detect(text, "Supervision|Probation|Circuit and District") == FALSE & header_flag == 1 & str_detect(table_name_full, "^Table E-2") == TRUE ~ "From Institutions", 
                                 TRUE ~ top_row_headers),
                     text = 
                       ifelse(is.na(top_row_headers) == TRUE, text, paste(top_row_headers, text, "_pivot", sep = " "))) %>% 
              dplyr::select(-top_row_headers) %>% 
              mutate(header_text = 
                       ifelse(header_flag == 1, text, NA)) %>% 
              group_by(col_id) %>% 
              tidyr::fill(header_text, .direction = "downup") %>% 
              ungroup() %>% 
              filter(!(header_flag == 1)) %>% 
              arrange(row_id, x) %>%
              mutate(page_flag = 
                       ifelse(str_detect(text, "Page") == TRUE & row_id == max(row_id, na.rm = TRUE), 1, NA),
                     notes_flag = 
                       ifelse(font_size >= 3 & font_size <= 6.5 | text == "*", 1, NA) %>% 
                       as.numeric()) %>%
              tidyr::fill(c(notes_flag, page_flag), .direction = "down") %>% 
              mutate(across(c(page_flag, notes_flag), 
                            ~.x %>% 
                              replace_na(0)),
                     notes_flag2 = 
                       ifelse(notes_flag == 1 | page_flag == 1, 0, 1) %>% 
                       lag(1) %>% 
                       replace_na(1) %>% 
                       cumsum(),
                     across(c(col_id, header_text, table_name_full, page_num), 
                            ~ifelse(notes_flag == 1 | page_flag == 1, NA, .x)),
                     text_numeric = 
                       text %>% 
                       as.numeric() %>% 
                       suppressWarnings()) %>% 
              filter(!(is.na(text_numeric) == FALSE & header_text == "Circuit and District")) %>% 
              dplyr::select(-text_numeric) %>%
              group_by(table_name_full, page_num, col_id, row_id, header_text, notes_flag, notes_flag2) %>% 
              dplyr::summarize(text = 
                                 text %>% 
                                 paste(collapse = " "),
                               font_size = 
                                 font_size %>% 
                                 min(na.rm = TRUE)) %>% 
              ungroup() %>% 
              mutate(pages_flag = 
                       ifelse(str_detect(text, "Page \\d+ of \\d+") == TRUE, 1, 0),
                     pages_flag = 
                       ifelse(str_detect(text, "Page of") == TRUE, 1, pages_flag),
                     row_id = 
                       ifelse(notes_flag == 1, NA, row_id)) %>% 
              filter(!(pages_flag == 1)) %>% 
              group_by(across(-c(text))) %>% 
              dplyr::summarize(text = 
                                 text %>% 
                                 paste(collapse = "; ")) %>% 
              ungroup() %>% 
              mutate(notes = 
                       ifelse(notes_flag == 1, text, NA)) %>% 
              tidyr::fill(notes, .direction = "updown") %>% 
              filter(!(notes_flag == 1)) %>% 
              dplyr::select(table_name_full, page_num, row_id, notes, text, header_text, col_id) %>%
              arrange(row_id, col_id) %>% 
              group_by(row_id, col_id) %>% 
              mutate(item_count = n(), 
                     item_count_count = 1:n()) %>% 
              ungroup() %>% 
              right_join(expand_grid(col_id = unique(.$col_id),
                                     row_id = unique(.$row_id)), 
                         by = c("row_id", "col_id")) %>% 
              arrange(row_id, col_id) %>% 
              mutate(text_lead = 
                       text %>% 
                       lead(1), 
                     col_id = 
                       ifelse(item_count > 1 & item_count_count == item_count & is.na(text_lead) == TRUE, col_id + 1, col_id), 
                     header_text = 
                       ifelse(item_count > 1 & item_count_count == item_count & is.na(text_lead) == TRUE, NA, header_text)) %>% 
              group_by(col_id) %>% 
              tidyr::fill(header_text, .direction = "downup") %>% 
              ungroup() %>% 
              dplyr::select(-c(item_count, item_count_count, text_lead)) %>% 
              filter(is.na(text) == FALSE) %>% 
              dplyr::select(-col_id) %>% 
              mutate(table_name_full = 
                       table_name_full %>% 
                       str_trim(side = "both") %>% 
                       na_if("")) %>% 
              tidyr::fill(table_name_full, .direction = "downup") %>% 
              pivot_wider(names_from = "header_text",
                          values_from = "text") %>%
              clean_names() %>% 
              rename_with(~.x %>% 
                            str_replace_all("circuit_and_district", "state_or_district")) %>% 
              mutate(circuit_and_district = 
                       ifelse(str_detect(state_or_district, "^[:digit:]") == TRUE, state_or_district, NA), 
                     across(where(is_character), 
                            ~.x %>% 
                              str_trim(side = "both") %>% 
                              na_if("NA"))) %>% 
              tidyr::fill(circuit_and_district, .direction = "down") %>%
              relocate(state_or_district, .after = "circuit_and_district") %>% 
              rename_with(~.x %>% 
                            str_replace_all("received_for_supervision_r_e_c_e_i_ve_d_by_transfer", "received_for_supervision_received_by_transfer") %>% 
                            str_replace_all("r_eceived", "received") %>% 
                            str_replace_all("received_for_supervision_s_p_e_c_i_al_parole", "received_for_supervision_special_parole")) %>% 
              rename_with(~.x %>% 
                            str_replace_all("persons_under_by_supervision", "persons_under_supervision") %>% 
                            str_replace_all("^persons_by_supervision", "persons_under_supervision")) %>% 
              rename_with(~ifelse(str_detect(.x, "persons_under_supervision|total") == TRUE, paste0(.x, "_pivot"), .x)) %>% 
              rename_with(~.x %>% 
                            str_replace_all("_pivot_pivot", "_pivot"))
        ) %>% 
        bind_rows() %>% 
        mutate(table_name_full = 
                 ifelse(page_num == 1, table_name_full, NA)) %>% 
        tidyr::fill(table_name_full, .direction = "updown") %>% 
        dplyr::select(-any_of(c("row_id", "page_num"))) %>% 
        tidyr::fill(notes, .direction = "updown") %>% 
        pivot_longer(cols = ends_with("_pivot"), 
                     names_to = "categories",
                     values_to = "values") %>% 
        mutate(values = 
                 values %>% 
                 str_replace_all(",", "") %>% 
                 str_trim(side = "both") %>% 
                 na_if("-") %>% 
                 as.numeric(),
               categories = 
                 categories %>% 
                 str_replace_all("_pivot$", "")) %>% 
        distinct() %>% 
        mutate(file_name = 
                 .x %>% 
                 basename() %>% 
                 str_replace_all("[.]pdf$", "")) %>% 
        select(where(~ !all(is.na(.x))))
  ) %>% 
  bind_rows() %>%
  mutate(table_name_full = 
           table_name_full %>% 
           str_replace_all("\u0097", "—") %>% 
           str_replace_all("System-", "System—")) %>% 
  separate_wider_delim(table_name_full, names = c("table_num", "table_desc"), delim = "—", cols_remove = FALSE, too_few = "align_start") %>%
  mutate(table_num = 
           table_num %>% 
           str_replace_all("^Table ", "") %>% 
           str_replace_all("[.]", "") %>% 
           str_replace_all("Federal Probation System", ""),
         date = 
           categories %>% 
           str_replace_all("arpil", "april") %>% 
           str_extract_all("[^_]+_[^_]+_[^_]+$") %>% 
           str_replace_all("_", " ") %>% 
           mdy() %>% 
           suppressWarnings(),
         original_categories = categories, 
         categories = 
           ifelse(str_detect(categories, "persons_under_supervision") == TRUE, "persons_under_supervision", categories) %>% 
           str_replace_all("^continued_immigration$", "offenses_continued_immigration") %>% 
           str_replace_all("^continued_other$", "offenses_continued_other") %>% 
           str_replace_all("^general_drugs$", "general_offenses_drugs") %>% 
           str_replace_all("_pivot$", "") %>% 
           str_replace_all("^general_total$", "general_offenses_total") %>% 
           str_replace_all("^institutions", "from_institutions") %>% 
           str_replace_all("s_p_e_c_i_al", "special") %>% 
           str_replace_all("received_transfer", "received_by_transfer") %>% 
           str_replace_all("^from_institutions_bureau_of$", "from_institutions_bop_custody") %>% 
           str_replace_all("bureau_of_prisons", "bop") %>% 
           str_replace_all("special_offenses_postal_laws", "special_offenses_postal") %>% 
           str_replace_all("^total$", "offenses_total"),
         values = 
           ifelse((categories == "persons_under_supervision" & state_or_district == "UT" & 
                     file_name == "e-1_2009-12-31" & date == "2009-12-31"), 0, values)) %>% 
  filter(is.na(values) == FALSE) %>%
  separate_wider_delim(file_name, names = c("file_name_table_num", "file_name_table_desc"), delim = "_", cols_remove = FALSE, too_few = "align_start") %>% 
  mutate(table_num = 
           ifelse(table_num == "", str_to_upper(file_name_table_num), table_num) %>% 
           str_trim(side = "both"), 
         across(where(is_character), 
                ~.x %>% 
                  na_if(""))) %>% 
  rename("table_name_date" = "file_name_table_desc") %>% 
  dplyr::select(-file_name_table_num) %>% 
  {
    df <- .
    df %>% 
      filter(!(categories == "persons_under_supervision" & state_or_district == "UT" & 
                 file_name == "e-1_2009-12-31" & date == "2009-12-31")) %>% 
      bind_rows(df %>% 
                  filter(categories == "persons_under_supervision" & state_or_district == "UT" & 
                           file_name == "e-1_2009-12-31" & date == "2009-12-31")) %>% 
      group_by(file_name, state_or_district) %>% 
      mutate(values_updated = 
               values %>% 
               lag(1)) %>% 
      ungroup() %>% 
      mutate(values = 
               case_when(categories == "removed_from_post_conviction_supervision_total_removed" & state_or_district == "UT" & 
                           file_name == "e-1_2009-12-31" ~ 670, 
                         state_or_district == "UT" & file_name == "e-1_2009-12-31" & 
                           str_detect(categories, "^removed_from_post_conviction_supervision") == TRUE ~ values_updated, 
                         categories == "persons_under_supervision" & state_or_district == "UT" & 
                           file_name == "e-1_2009-12-31" & date == "2009-12-31" ~ values_updated, 
                         TRUE ~ values))
  }
  
##Combine Data
full_data <- 
  xlsx_data %>% 
  mutate(file_type = "XLSX") %>% 
  bind_rows(pdf_data %>% 
              dplyr::select(-values_updated) %>% 
              mutate(file_type = "PDF")) %>% 
  mutate(categories = 
           categories %>% 
           str_replace_all("parole_special_parole", "special_parole") %>% 
           str_replace_all("drug_laws", "drugs") %>% 
           str_replace_all("immigration_laws", "immigration") %>% 
           str_replace_all("_continued_", "") %>% 
           str_replace_all("offenses", "offenses_") %>% 
           str_replace_all("__", "_") %>% 
           str_replace_all("_$", "") %>% 
           str_replace_all("presentence_total", "presentence_investigations_total") %>% 
           str_replace_all("^received_for_supervision", "received_for_post_conviction_supervision") %>% 
           str_replace_all("^removed_from_supervision", "removed_from_post_conviction_supervision") %>% 
           str_replace_all("^general_offenses", "offenses") %>% 
           str_replace_all("^offenses_firearms$", "offenses_weapons_and_firearms") %>% 
           str_replace_all("received_for_post_conviction_supervision_term_of_release", "received_for_post_conviction_supervision_term_of_supervised_release") %>% 
           str_replace_all("received_for_post_conviction_supervision_supervised_release", "received_for_post_conviction_supervision_term_of_supervised_release") %>% 
           str_replace_all("removed_from_post_conviction_supervision_supervised_release", "removed_from_post_conviction_supervision_term_of_supervised_release") %>% 
           str_replace_all("received_for_post_conviction_supervision_supervised_probation", "received_for_post_conviction_supervision_probation") %>% 
           str_replace_all("from_courts_judge_probation", "from_courts_probation") %>% 
           str_replace_all("from_courts_magistrate_judge_probation", "from_courts_probation") %>% 
           str_replace_all("from_courts_term_of_supervised_release", "from_institutions_term_of_supervised_release") %>% 
           str_replace_all("from_institutions_mandatory_release", "from_institutions_bop_custody") %>% 
           str_replace_all("from_institutions_military_parole", "from_institutions_parole") %>% 
           str_replace_all("from_institutions_special_parole", "from_institutions_parole") %>% 
           str_replace_all("from_institutions_prisons_custody", "from_institutions_bop_custody") %>% 
           str_replace_all("from_institutions_total", "persons_under_supervision") %>% 
           str_replace_all("received_for_post_conviction_supervision_judge_probation", "received_for_post_conviction_supervision_probation") %>% 
           str_replace_all("received_for_post_conviction_supervision_magistrate_judge_probation", "received_for_post_conviction_supervision_probation") %>% 
           str_replace_all("received_for_post_conviction_supervision_mandatory_release", "received_for_post_conviction_supervision_bop_custody") %>% 
           str_replace_all("received_for_post_conviction_supervision_military_parole", "received_for_post_conviction_supervision_parole") %>% 
           str_replace_all("received_for_post_conviction_supervision_special_parole", "received_for_post_conviction_supervision_parole") %>% 
           str_replace_all("received_for_post_conviction_supervision_term_of_parole", "received_for_post_conviction_supervision_parole") %>% 
           str_replace_all("removed_from_post_conviction_supervision_judge_probation", "removed_from_post_conviction_supervision_probation") %>% 
           str_replace_all("removed_from_post_conviction_supervision_magistrate_judge_probation", "removed_from_post_conviction_supervision_probation") %>% 
           str_replace_all("removed_from_post_conviction_supervision_mandatory_release", "removed_from_post_conviction_supervision_bop_custody") %>% 
           str_replace_all("removed_from_post_conviction_supervision_military_parole", "removed_from_post_conviction_supervision_parole") %>% 
           str_replace_all("removed_from_post_conviction_supervision_special_parole", "removed_from_post_conviction_supervision_parole") %>% 
           str_replace_all("removed_from_post_conviction_supervision_term_of_parole", "removed_from_post_conviction_supervision_parole")) %>% 
  rename("variables" = "categories") %>% 
  mutate(categories = 
           variables %>% 
           str_extract("^from_courts|^from_institutions|^general_offenses|^offenses|^persons_under_supervision|^presentence_investigations|^received_for_post_conviction_supervision|^received_for_supervision|^removed_from_post_conviction_supervision|^removed_from_supervision|^special_offenses|^total_closed") %>% 
           str_replace_all("total_closed", "post_conviction_supervision_cases_closed") %>% 
           str_replace_all("_", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Post Conviction", "Post-Conviction") %>% 
           str_replace_all("Presentence", "Pre-Sentence"),
         variables_simple = 
           variables %>% 
           str_replace_all("^from_courts|^from_institutions|^general_offenses|^offenses|^persons_under_supervision|^presentence_investigations|^received_for_post_conviction_supervision|^received_for_supervision|^removed_from_post_conviction_supervision|^removed_from_supervision|^special_offenses|^total_closed", "") %>% 
           str_replace_all("^_", "") %>% 
           na_if("")) %>% 
  mutate(state_or_district = 
           case_when(state_or_district %in% c("A,N", "IIA,N") & 
                       (circuit_and_district == "8th" | circuit_and_district == "8TH") ~ "IA,N", 
                     state_or_district %in% c("A,S", "IIA,S") & 
                       (circuit_and_district == "8th" | circuit_and_district == "8TH") ~ "IA,S", 
                     state_or_district == "Y,W" & 
                       (circuit_and_district == "2nd" | circuit_and_district == "2ND") ~ "NY,W", 
                     state_or_district == "^1TH$" ~ "11TH", 
                     state_or_district == "GUAM" ~ "GU", 
                     state_or_district == "NMI" ~ "MP",
                     state_or_district == "NM,I" ~ "MP",
                     TRUE ~ state_or_district),
         across(where(is_character), 
                ~.x %>% 
                  str_replace_all("^1TH$", "11TH") %>% 
                  str_replace_all("^1th", "11th")),
         across(c(state_or_district, circuit_and_district), 
                ~ifelse(str_detect(.x, "^[:digit:]") == TRUE, str_to_title(.x), str_to_upper(.x)))) %>% 
  filter(is.na(values) == FALSE) %>% 
  filter(!(variables == "presentence_investigations_total" & table_num == "E-3")) %>% 
  group_by(across(-c(values))) %>% 
  dplyr::summarize(values = 
                     values %>% 
                     sum(na.rm = TRUE)) %>% 
  ungroup()


####Write Data####
##Create Schema
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS us_courts;")

##Insert into Database
dbWriteTable(con, DBI::Id("us_courts", "us_federal_courts_data"), full_data, overwrite = TRUE)

# ##Write CSV
# fwrite(full_data, here("data", "us_federal_courts_data.csv"), row.names = FALSE)

##Delete CSV
"data" %>% 
  here() %>%
  list.files(full.names = TRUE) %>% 
  str_subset("us_federal_courts_data.csv.gz$") %>% 
  unlink()

##Write Compressed CSV
fwrite(full_data, here("data", "us_federal_courts_data.csv.gz"), compress = "gzip")


####Fix Shapefile Data####
##Circuit Shapefile Data
judicial_circuit_sf <- 
  "data" %>% 
  here("judicial_sf_data.rds") %>% 
  read_rds() %>% 
  group_by(circuit) %>% 
  dplyr::summarize(population = 
                     population %>% 
                     sum(na.rm = TRUE),
                   geometry = 
                     geometry %>% 
                     st_make_valid() %>% 
                     st_union()) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  mutate(geometry = 
           geometry %>% 
           st_cast("MULTIPOLYGON") %>% 
           st_make_valid()) %>% 
  mutate(circuit_numeric = 
           circuit %>% 
           as.numeric() %>% 
           suppressWarnings() %>% 
           scales::ordinal(),
         circuit = 
           ifelse(is.na(circuit_numeric) == FALSE, circuit_numeric, circuit)) %>% 
  dplyr::select(-circuit_numeric)

write_rds(judicial_circuit_sf, here("data", "judicial_circuit_sf.rds"))


  







