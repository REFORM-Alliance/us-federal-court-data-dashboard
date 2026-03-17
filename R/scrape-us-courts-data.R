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


####Utility Functions####
##Find Free Port Function
find_free_port <- function(start_port = 4000, end_port = 9000, sudo_password){
  tried_ports <- integer(0)
  port_range <- start_port:end_port
  
  while (length(tried_ports) < length(port_range)){
    # Randomly choose a port not already tried
    remaining_ports <- setdiff(port_range, tried_ports)
    port <- sample(remaining_ports, 1)
    tried_ports <- c(tried_ports, port)
    
    # Build the command
    cmd <- sprintf("echo '%s' | sudo -S lsof -i :%d", sudo_password, port)
    
    # Execute the command
    res <- suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE))
    
    if (length(res) == 0){
      return(port)
    }
  }
  
  stop("No free port found in range.")
}

##Kill Selenium Processes
kill_selenium <- function(port_to_kill){
  cmd_pid <- sprintf("lsof -ti tcp:%d 2>/dev/null", port_to_kill)
  pids <- tryCatch({
    pid_output <- system(cmd_pid, intern = TRUE)
    as.integer(pid_output)
  },
  error = function(e) NA
  )
  
  if(length(pids) == 0){
    message(sprintf("No processes found on port %d", port_to_kill))
    return(invisible(NULL))
  }
  
  for(pid in as.integer(pids)){
    cmd_args <- sprintf("ps -p %d -o args=", pid)
    args <- 
      tryCatch(system(cmd_args, intern = TRUE), error = function(e) "") %>%
      suppressWarnings()
    if(length(args) == 0){
      next
    }else if(grepl("selenium-server-standalone", args, ignore.case = TRUE)){
      cmd_kill <- sprintf("kill %d 2>/dev/null", pid)
      system(cmd_kill)
    }
  }
}


####Start Scraper####
##Start Selenium
free_port = find_free_port(sudo_password = Sys.getenv("SUDO_PASS"))
system(glue("nohup java -jar /home/ubuntu/selenium-server-standalone-3.141.59.jar -port {free_port} > logs/selenium.log 2>&1 &"))
Sys.sleep(2)

##Rotating User-Agent pool
ua_list <- c(
  "Mozilla/5.0(Windows NT 10.0; Win64; x64) AppleWebKit/537.36(KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  "Mozilla/5.0(Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15(KHTML, like Gecko) Version/17.0 Safari/605.1.15",
  "Mozilla/5.0(X11; Linux x86_64) AppleWebKit/537.36(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)
useragent <- sample(ua_list, 1)

##Set Chrome preferences
chrome_args <- c(
  "--headless",
  "--disable-gpu",
  "--no-sandbox",
  "--disable-dev-shm-usage",
  paste0("--user-agent=", useragent) # Apply the random user agent here
)

##Start Chrome Driver
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = free_port,
  browserName = "chrome",
  extraCapabilities = list(
    chromeOptions = list(
      binary = "/usr/bin/google-chrome",
      args = chrome_args
    )
  )
)

tryCatch(
  invisible(capture.output(remDr$open(), type = "output")),
  error = function(e) print(e)
)

##Go to Statistics Report Search Page 
if(exists("remDr")){
  remDr$navigate("https://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables")
  page_title <- remDr$getTitle()
}else{
  cat("Failed to open the remote browser session. Check for errors.\n")
}

##Get Table Numbers
table_numbers <- 
  data.frame("table_number" = c(1:4, 7:8, 10)) %>% 
  mutate(table_letter = 
           ifelse(table_number %in% c(7:8), "A, B", "B")) %>% 
  separate_rows(table_letter,
                sep = ",\\s*") %>% 
  mutate(table_number_full = 
           table_number %>% 
           paste0(table_letter) %>% 
           str_replace_all("B", ""),
         table_number_full = 
           paste0("E-", table_number_full)) %>% 
  pull(table_number_full) %>% 
  paste(collapse = ", ")


##Search by Table Number
# search_by_table_number <- remDr$findElement(using = "name", value = "tn")
search_by_table_number <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/main/div[1]/div/div[3]/div[2]/article/div/div[2]/div/div/div/div[1]/form/div/div[1]/input")
search_by_table_number$sendKeysToElement(list(table_numbers))

Sys.sleep(1)

search_by_topic <- remDr$findElement(using = "name", value = "term_node_tid_depth")
search_by_topic$clickElement()
search_by_topic_option <- remDr$findElement(using = "xpath", value = paste0("//option[text()='", "-Probation", "']"))
search_by_topic_option$clickElement()

Sys.sleep(1)

search_button <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/main/div[1]/div/div[3]/div[2]/article/div/div[2]/div/div/div/div[1]/form/div/div[6]/input[1]")
search_button$clickElement()

# all_links <- remDr$findElements(using = 'tag name', value = 'a')

table_element <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/main/div[1]/div/div[3]/div[2]/article/div/div[2]/div/div/div/div[2]/div/table")
table_html <- table_element$getElementAttribute("outerHTML")[[1]]

Sys.sleep(1)

all_href_elements <- remDr$findElements(using = "xpath", value = "//*[@href]")
urls <- 
  all_href_elements %>% 
  sapply(function(x) x$getElementAttribute("href")) %>%
  unlist() %>%
  na.omit() %>% 
  str_subset("[.]xlsx$")

tbl <- 
  table_html %>%
  read_html() %>%
  html_table() %>% 
  as.data.frame() %>% 
  mutate(url = urls)

total_pages <- length(remDr$findElements(using = "class name", value = "usa-pagination__item")) - 1

pages_vec <- (1:total_pages) - 1

full_df <- 
  pages_vec %>% 
  map(~{
    page_num = .x
    url = glue::glue("https://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables?m=All&pn=All&term_node_tid_depth=70&tn=E-1%2C%20E-2%2C%20E-3%2C%20E-4%2C%20E-7A%2C%20E-7%2C%20E-8A%2C%20E-8%2C%20E-10&y%5Bmax%5D=&y%5Bmin%5D=&y%5Bmonth_between%5D=&y%5Byear_between%5D=&page={page_num}")
    
    if(exists("remDr")){
      remDr$navigate(url)
      page_title <- remDr$getTitle()
    }else{
      cat("Failed to open the remote browser session. Check for errors.\n")
    }
    
    Sys.sleep(1)
    
    table_numbers_isolated <- 
      table_numbers %>% 
      str_split(pattern = ", ") %>% 
      unlist() %>% 
      str_to_lower() %>% 
      str_replace_all("-", "") %>% 
      str_replace_all("a|b|^e", "") %>% 
      unique()
    
    table_numbers_isolated2 <- unique(c(table_numbers_isolated, str_pad(table_numbers_isolated, width = 2, side = "left", pad = "0")))
    table_numbers_isolated3 <- paste(c(paste0("E", table_numbers_isolated2), paste0("e", table_numbers_isolated2)), collapse = "|")
    
    all_href_elements <- remDr$findElements(using = "xpath", value = "//*[@href]")
    urls <- 
      all_href_elements %>% 
      sapply(function(x) x$getElementAttribute("href")) %>%
      unlist() %>%
      na.omit() %>% 
      str_subset("[.]xlsx$|[.]pdf$") %>% 
      str_subset(table_numbers_isolated3)
    
    urls_df <- 
      data.frame(urls = urls) %>% 
      mutate(file_name = 
               urls %>%
               basename(), 
             base_url = 
               urls %>% 
               dirname(),
             file_ending = 
               file_name %>% 
               str_extract_all("[.]pdf$|[.]xlsx$") %>% 
               str_replace_all("[.]", ""), 
             file_ending_num = 
               ifelse(file_ending == "pdf", 0, 1), 
             file_name_stripped = 
               file_name %>% 
               str_replace_all("[.]pdf$|[.]xlsx$", "")) %>% 
      group_by(file_name_stripped) %>% 
      filter(file_ending_num == max(file_ending_num, na.rm = TRUE)) %>% 
      ungroup()
    
    urls <- 
      urls_df %>% 
      pull(urls)
    
    Sys.sleep(1)
    
    table_element <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/main/div[1]/div/div[3]/div[2]/article/div/div[2]/div/div/div/div[2]/div/table")
    table_html <- table_element$getElementAttribute("outerHTML")[[1]]
    
    tbl <- 
      table_html %>%
      read_html() %>%
      html_table() %>% 
      as.data.frame() %>% 
      mutate(url = urls) %>% 
      clean_names()
    
    return(tbl)
  }) %>% 
  bind_rows() %>% 
  mutate(reporting_date_clean = 
           reporting_period %>% 
           as.Date(format = "%B %d, %Y"),
         file_name = 
           table_number %>% 
           str_to_lower() %>% 
           paste(reporting_date_clean, 
                 sep = "_") %>% 
           paste(url %>% 
                   str_extract_all("pdf$|xlsx$"), 
                 sep = "."), 
         file_name_full = 
           "data-raw" %>%
           here("usaoc-data-tables") %>% 
           here(file_name)) %>% 
  dplyr::select(-starts_with("var")) %>% 
  distinct()
  
kill_selenium(port_to_kill = free_port)

##Download all of the PDFs and XLSXs
files_read <- 
  here("data-raw") %>% 
  here("usaoc-data-tables") %>% 
  list.files(full.names = TRUE)
files_to_read <- setdiff(files_read, full_df$file_name_full)

while(length(files_to_read) > 0){
  full_df %>% 
    dplyr::select(url, file_name_full) %>% 
    as.data.frame() %>% 
    pwalk(~curl::curl_download(.x, .y))
  
  files_read <- 
    here("data-raw", "usaoc-data-tables") %>% 
    list.files(full.names = TRUE)
  
  files_to_read <- setdiff(files_read, full_df$file_name_full)
}


# ####Parse Data####
# ##XLSX
# xlsx_data <- 
#   "data-raw" %>% 
#   here() %>% 
#   list.files(full.names = TRUE) %>% 
#   str_subset("xlsx$") %>% 
#   str_subset("e-7", negate = TRUE) %>% 
#   map(~.x %>% 
#         read_excel(col_names = FALSE) %>%
#         mutate(file_name = 
#                  .x %>% 
#                  basename() %>% 
#                  str_replace_all("[.]xlsx$", ""),
#                across(where(is_character), 
#                       ~.x %>% 
#                         str_replace_all("\r\n", " ") %>% 
#                         str_replace_all("\n", " ") %>% 
#                         str_replace_all("  ", " "))) %>% 
#         clean_names() %>% 
#         select(where(~ !all(is.na(.)))) %>% 
#         mutate(table_name = 
#                  ifelse(str_detect(x1, "Table E-") == TRUE, x1, NA)) %>% 
#         tidyr::fill(table_name, .direction = "down") %>% 
#         mutate(x1 = 
#                  x1 %>% 
#                  replace_na("NA")) %>% 
#         filter(!(str_detect(x1, "Table E-") == TRUE)) %>% 
#         mutate(x1 = 
#                  x1 %>% 
#                  na_if("NA"),
#                row_header_flag = 
#                  ifelse(x1 == "Circuit and District", 1, NA)) %>% 
#         tidyr::fill(row_header_flag, .direction = "down") %>%
#         group_by(row_header_flag) %>% 
#         mutate(row_header_flag = 
#                  row_header_flag %>% 
#                  cumsum()) %>% 
#         ungroup() %>% 
#         rownames_to_column("row_num") %>% 
#         mutate(row_num = 
#                  row_num %>% 
#                  as.numeric()) %>% 
#         filter(!if_all(starts_with("x"), is.na)) %>% 
#         
#         mutate(row_header_flag = 
#                  ifelse(x1 == "TOTAL", 1, NA)) %>% 
#         tidyr::fill(row_header_flag, .direction = "down") %>% 
#         mutate(row_header_flag = 
#                  row_header_flag %>% 
#                  replace_na(0) %>% 
#                  cumsum()) %>% 
#         
#         group_by(across(-c(x1, row_num))) %>%
#         dplyr::summarize(x1 =
#                            x1 %>%
#                            paste(collapse = " "),
#                          row_num =
#                            row_num %>%
#                            min(na.rm = TRUE)) %>%
#         ungroup() %>%
#         mutate(x1 =
#                  x1 %>%
#                  na_if("NA")) %>%
#         arrange(row_num) %>%
#         mutate(table_desc =
#                  ifelse(row_num == 1, x1, NA), 
#                table_desc_new =
#                  ifelse(row_num == 1 & table_desc == "Circuit and District", NA, table_desc),
#                table_desc = table_desc_new) %>% 
#         dplyr::select(-table_desc_new) %>%
#         tidyr::fill(table_desc, .direction = "down") %>%
#         mutate(row_num_new = case_when(row_num == 1 & x1 == "Circuit and District" & is.na(table_desc) == TRUE ~ row_num, 
#                                        row_num == 1 & is.na(table_desc) == FALSE ~ NA, 
#                                        row_num > 1 ~ row_num, 
#                                        TRUE ~ NA),
#                row_num = row_num_new) %>% 
#         dplyr::select(-row_num_new) %>% 
#         filter(is.na(row_num) == FALSE) %>%
#         mutate(row_header_flag =
#                  ifelse(x1 == "TOTAL", 1, NA)) %>%
#         tidyr::fill(row_header_flag, .direction = "down") %>%
#         group_by(row_header_flag) %>%
#         mutate(row_header_flag =
#                  row_header_flag %>%
#                  cumsum()) %>%
#         ungroup() %>%
#         mutate(row_header_flag =
#                  row_header_flag %>%
#                  replace_na(0)) %>%
#         group_by(row_header_flag) %>%
#         mutate(row_num = ifelse(row_header_flag == 0, row_num - min(row_num, na.rm = TRUE) + 1, row_num)) %>%
#         ungroup() %>%
#         relocate(x1, .before = "x2") %>%
#         mutate(table_desc = 
#                  table_desc %>% 
#                  replace_na("Missing")) %>% 
#         t() %>%
#         as.data.frame() %>%
#         rownames_to_column(var = "row_names") %>%
#         mutate(row_names_new =
#                  row_names %>%
#                  str_extract_all("^x")) %>%
#         group_by(row_names_new) %>%
#         tidyr::fill(V1, .direction = "down") %>%
#         ungroup() %>%
#         dplyr::select(-row_names_new) %>%
#         t() %>%
#         as.data.frame() %>%
#         row_to_names(row_number = 1) %>%
#         group_by(row_header_flag) %>%
#         dplyr::summarize(across(starts_with("x"),
#                                 ~.x %>%
#                                   na.omit() %>%
#                                   paste(collapse = " ")),
#                          across(c(!starts_with("x"), -row_num),
#                                 ~.x %>%
#                                   unique()),
#                          row_num =
#                            row_num %>%
#                            min(na.rm = TRUE)) %>%
#         ungroup() %>%
#         dplyr::select(-row_num) %>%
#         mutate(across(where(is_character),
#                       ~.x %>%
#                         na_if("")),
#                row_header_flag =
#                  row_header_flag %>%
#                  as.numeric(),
#                across(c("file_name", "table_name", "table_desc"),
#                       ~ifelse(row_header_flag == 0, cur_column(), .x))) %>%
#         dplyr::select(-row_header_flag) %>%
#         mutate(row_num = row_number(), 
#                across(where(is_character), 
#                       ~ifelse(row_num == 1, 
#                               .x %>% 
#                                 str_trim(side = "both") %>% 
#                                 str_replace_all("[\u00B9-\u00B9\u00B2-\u00B3\u2070-\u2079]$", "") %>% 
#                                 str_remove("(?<=\\D)\\d$"),
#                               .x))) %>% 
#         dplyr::select(-row_num) %>% 
#         row_to_names(row_number = 1) %>%
#         clean_names() %>%
#         select(where(~ !all(is.na(.)))) %>% 
#         mutate(state_or_district =
#                  ifelse(is.na(circuit_and_district) == TRUE, circuit_and_district_2, circuit_and_district)) %>%
#         dplyr::select(-contains("circuit_and_district_2")) %>%
#         filter(is.na(state_or_district) == FALSE) %>%
#         mutate(circuit_and_district = 
#                  ifelse(str_detect(circuit_and_district, "^[:digit:]") == TRUE, circuit_and_district, NA)) %>% 
#         tidyr::fill(circuit_and_district, .direction = "down") %>%
#         relocate(state_or_district, .after = "circuit_and_district") %>%
#         mutate(notes_flag =
#                  ifelse(str_detect(state_or_district, "^[:digit:] |^[\u00B9-\u00B9\u00B2-\u00B3\u2070-\u2079] ") == TRUE, 0, 1),
#                state_or_district =
#                  ifelse(notes_flag == 0, NA, state_or_district),
#                notes_flag_lag =
#                  notes_flag %>%
#                  lag(1) %>%
#                  replace_na(0),
#                notes_flag_sum =
#                  notes_flag + notes_flag_lag,
#                notes_flag_max =
#                  ifelse(notes_flag_sum > 1, 1, notes_flag_sum),
#                notes_flag_cumsum =
#                  notes_flag_max %>%
#                  cumsum()) %>%
#         dplyr::select(-c("notes_flag_lag", "notes_flag_sum", "notes_flag_max")) %>%
#         mutate(notes_flag =
#                  ifelse(notes_flag == 0, 1, 0)) %>%
#         group_by(across(-c(circuit_and_district))) %>%
#         dplyr::summarize(circuit_and_district =
#                            circuit_and_district %>%
#                            paste(collapse = "; "),
#                          notes_flag =
#                            notes_flag %>%
#                            max(na.rm = TRUE)) %>%
#         ungroup() %>%
#         relocate(circuit_and_district, .before = 1) %>%
#         arrange(notes_flag_cumsum) %>%
#         mutate(notes =
#                  ifelse(notes_flag == 1, circuit_and_district, NA)) %>%
#         group_by(file_name, table_name, table_desc) %>%
#         tidyr::fill(notes, .direction = "updown") %>%
#         ungroup() %>%
#         mutate(across(contains("supervision"),
#                       ~.x %>%
#                         as.numeric())) %>% 
#         filter(is.na(state_or_district) == FALSE) %>% 
#         dplyr::select(-c(notes_flag, notes_flag_cumsum)) %>% 
#         distinct() %>% 
#         mutate(across(where(is_character), 
#                       ~.x %>% 
#                         na_if("NA") %>% 
#                         na_if("Missing"))) %>% 
#         rename_with(~.x %>% 
#                       str_replace_all("^total$", "presentence_total")) %>% 
#         mutate(across(-any_of(c("circuit_and_district", "state_or_district", 
#                                 "file_name", "table_name", "table_desc", "notes")), 
#                       ~.x %>% 
#                         as.numeric())) %>% 
#         select(where(~ !all(is.na(.)))) %>% 
#         pivot_longer(cols = -any_of(c("circuit_and_district", "state_or_district", 
#                                       "file_name", "table_name", "table_desc", "notes")),
#                      names_to = "categories",
#                      values_to = "values") 
#   ) %>% 
#   bind_rows() %>% 
#   relocate(c("file_name", "table_name", "table_desc"), .after = "state_or_district") %>% 
#   relocate("notes", .after = last_col()) %>% 
#   mutate(table_name_full = 
#            ifelse(is.na(table_desc) == TRUE, table_name, paste0(table_name, table_desc, sep = " ")) %>% 
#            str_replace_all(" -- ", "—") %>% 
#            str_replace_all(" ---- ", "—") %>% 
#            str_replace_all(" —", "—") %>% 
#            str_replace_all("[.] ", ".") %>% 
#            str_replace_all(" - ", "—") %>% 
#            str_replace_all("— ", "—") %>% 
#            str_trim(side = "both")) %>% 
#   dplyr::select(-c(table_desc, table_name)) %>% 
#   separate_wider_delim(table_name_full, names = c("table_num", "table_desc"), delim = "—", cols_remove = FALSE) %>%
#   mutate(table_num = 
#            table_num %>% 
#            str_replace_all("^Table ", "") %>% 
#            str_replace_all("[.]", "") %>% 
#            str_replace_all("Federal Probation System", ""),
#          date = 
#            categories %>% 
#            str_extract_all("[^_]+_[^_]+_[^_]+$") %>% 
#            str_replace_all("_", " ") %>% 
#            mdy() %>% 
#            suppressWarnings())
# 
# ##PDF
# y_tolerance <- 8
# x_tolerance <- 35
# max_col_nums <- 9
# 
# pdf_data <- 
#   "data-raw" %>% 
#   here() %>% 
#   list.files(full.names = TRUE) %>% 
#   str_subset("pdf$") %>% 
#   str_subset("e-7", negate = TRUE) %>% 
#   str_subset("e-1") %>% 
#   str_subset("e-10", negate = TRUE) %>% 
#   # head(2) %>% 
#   # tail(1) %>% 
#   head(2) %>% 
#   map(~.x %>% 
#         pdf_data(font_info = TRUE) %>% 
#         set_names(c(1:as.numeric(length(.)))) %>% 
#         map(~.x %>% 
#               mutate(table_name_flag = 
#                        ifelse(font_size == max(font_size, na.rm = TRUE), 1, 0),
#                      row_name_flag = 
#                        ifelse(table_name_flag == 0, 1, 0) %>% 
#                        cumsum()) %>% 
#               group_by(table_name_flag) %>% 
#               mutate(margin_x = 
#                        ifelse(table_name_flag == 1, min(x, na.rm = TRUE), NA)) %>% 
#               ungroup() %>% 
#               tidyr::fill(margin_x, .direction = "downup") %>% 
#               filter(!(x <= margin_x - 15)) %>% 
#               group_by(row_name_flag, font_name) %>%
#               dplyr::summarize(across(c(where(is.numeric), where(is.integer)), 
#                                       ~.x %>% 
#                                         max(na.rm = TRUE)),
#                                text = 
#                                  text %>% 
#                                  paste(collapse = " ")) %>% 
#               ungroup() %>% 
#               mutate(table_name_full = 
#                        ifelse(table_name_flag == 1, text, NA)) %>% 
#               tidyr::fill(table_name_full, .direction = "down") %>% 
#               filter(!(table_name_flag == 1)) %>% 
#               dplyr::select(-table_name_flag) %>% 
#               mutate(text = 
#                        text %>% 
#                        str_trim(side = "both") %>% 
#                        na_if("")) %>% 
#               filter(is.na(text) == FALSE) %>%  
#               arrange(x) %>% 
#               # mutate(col_id = 
#               #          x %>% 
#               #          dist() %>% 
#               #          hclust(method = "centroid") %>% 
#               #          cutree(k = max_col_nums)) %>% 
#               mutate(col_id = 
#                        x %>% 
#                        dist() %>% 
#                        hclust(method = "centroid") %>% 
#                        cutree(h = x_tolerance)) %>% 
#               arrange(y) %>% 
#               mutate(row_id = 
#                        y %>% 
#                        dist() %>% 
#                        hclust(method = "centroid") %>% 
#                        cutree(h = y_tolerance),
#                      row_id_1 = 
#                        ifelse(row_id == 1, 0, 1) %>% 
#                        cumsum()) %>% 
#               group_by(row_id_1, font_name, table_name_full) %>% 
#               dplyr::summarize(across(c(where(is.numeric), where(is.integer)), 
#                                       ~.x %>% 
#                                         min(na.rm = TRUE)),
#                                text = 
#                                  text %>% 
#                                  paste(collapse = " ")) %>% 
#               ungroup() %>% 
#               mutate(text = 
#                        text %>% 
#                        str_trim(side = "both"))) %>%
#         bind_rows(.id = "page_num") %>% 
#         mutate(first_table_row = 
#                  ifelse(col_id == 1 & text == "TOTAL", row_id, NA)) %>% 
#         tidyr::fill(first_table_row, .direction = "updown") %>% 
#         mutate(header_flag = 
#                  ifelse(row_id < first_table_row, 1, 0)) %>% 
#         group_split(page_num) %>% 
#         map(~.x %>% 
#               arrange(desc(header_flag), row_id) %>% 
#               mutate(header_flag2 = 
#                        ifelse(header_flag == 1, 0, 1) %>% 
#                        cumsum(), 
#                      top_row_headers = 
#                        ifelse(row_id == 1 & header_flag == 1, text, NA)) %>% 
#               group_by(header_flag) %>% 
#               tidyr::fill(top_row_headers, .direction = "down") %>% 
#               ungroup() %>% 
#               filter(!(row_id == 1 & header_flag == 1)) %>% 
#               # mutate(subscript_flag = 
#               #          ifelse(header_flag == 1 & font_size >= 3 & font_size != max(font_size, na.rm = TRUE), 1, 0))
#               filter(!(header_flag == 1 & font_size >= 3 & font_size <= 6.5)) %>%
#               group_by(font_name, table_name_full, page_num, header_flag2, col_id, top_row_headers) %>% 
#               dplyr::summarize(across(c(where(is.numeric), where(is.integer)), 
#                                       ~.x %>% 
#                                         min(na.rm = TRUE)),
#                                text = 
#                                  text %>% 
#                                  paste(collapse = " ")) %>% 
#               ungroup() %>% 
#               mutate(top_row_headers = 
#                        ifelse(text == "Circuit and District" | str_detect(text, "Supervision") == TRUE, NA, top_row_headers), 
#                      text = 
#                        ifelse(is.na(top_row_headers) == TRUE, text, paste(top_row_headers, text, sep = " "))) %>% 
#               dplyr::select(-top_row_headers) %>% 
#               mutate(header_text = 
#                        ifelse(header_flag == 1, text, NA)) %>% 
#               group_by(col_id) %>% 
#               tidyr::fill(header_text, .direction = "downup") %>% 
#               ungroup() %>% 
#               filter(!(header_flag == 1)) %>% 
#               arrange(row_id, x) %>%
#               mutate(page_flag = 
#                        ifelse(str_detect(text, "Page") == TRUE & row_id == max(row_id, na.rm = TRUE), 1, NA),
#                      # max_font_size =
#                      #   font_size %>%
#                      #   max(na.rm = TRUE),
#                      # min_font_size = 
#                      #   font_size %>% 
#                      #   max(na.rm = TRUE),
#                      # notes_flag =
#                      #   ifelse(font_size == min_font_size & font_size != max_font_size & min_font_size != max_font_size, 1, NA) %>% 
#                      #   as.numeric(),
#                      notes_flag = 
#                        ifelse(font_size >= 3 & font_size <= 6.5, 1, NA) %>% 
#                        as.numeric()) %>%
#               tidyr::fill(c(notes_flag, page_flag), .direction = "down") %>% 
#               mutate(across(c(page_flag, notes_flag), 
#                             ~.x %>% 
#                               replace_na(0)),
#                      notes_flag2 = 
#                        ifelse(notes_flag == 1 | page_flag == 1, 0, 1) %>% 
#                        lag(1) %>% 
#                        replace_na(1) %>% 
#                        cumsum(),
#                      across(c(col_id, header_text, table_name_full, page_num), 
#                             ~ifelse(notes_flag == 1 | page_flag == 1, NA, .x))) %>% 
#               group_by(table_name_full, page_num, col_id, row_id, header_text, notes_flag, notes_flag2) %>% 
#               dplyr::summarize(text = 
#                                  text %>% 
#                                  paste(collapse = " "),
#                                font_size = 
#                                  font_size %>% 
#                                  min(na.rm = TRUE)) %>% 
#               ungroup() %>% 
#               mutate(pages_flag = 
#                        ifelse(str_detect(text, "Page \\d+ of \\d+") == TRUE, 1, 0),
#                      row_id = 
#                        ifelse(notes_flag == 1, NA, row_id)) %>% 
#               filter(!(pages_flag == 1)) %>% 
#               group_by(across(-c(text))) %>% 
#               dplyr::summarize(text = 
#                                  text %>% 
#                                  paste(collapse = "; ")) %>% 
#               ungroup() %>% 
#               mutate(notes = 
#                        ifelse(notes_flag == 1, text, NA)) %>% 
#               tidyr::fill(notes, .direction = "updown") %>% 
#               filter(!(notes_flag == 1)) %>% 
#               dplyr::select(table_name_full, page_num, row_id, notes, text, header_text) %>%
#               pivot_wider(names_from = "header_text",
#                           values_from = "text") %>%
#               clean_names() %>% 
#               mutate(state_or_district = 
#                        circuit_and_district,
#                      circuit_and_district = 
#                        ifelse(str_detect(circuit_and_district, "^[:digit:]") == TRUE, circuit_and_district, NA), 
#                      across(where(is_character), 
#                             ~.x %>% 
#                               str_trim(side = "both") %>% 
#                               na_if("NA"))) %>% 
#               tidyr::fill(circuit_and_district, .direction = "down") %>%
#               relocate(state_or_district, .after = "circuit_and_district")
#             ) %>% 
#         bind_rows() %>% 
#         dplyr::select(-c(row_id, page_num)) %>% 
#         tidyr::fill(notes, .direction = "updown") %>% 
#         pivot_longer(cols = contains("supervision"), 
#                      names_to = "categories",
#                      values_to = "values") %>% 
#         mutate(values = 
#                  values %>% 
#                  str_replace_all(",", "") %>% 
#                  str_trim(side = "both") %>% 
#                  na_if("-") %>% 
#                  as.numeric()) %>% 
#         filter(is.na(values) == FALSE) %>% 
#         distinct() %>% 
#         mutate(file_name = 
#                  .x %>% 
#                  basename() %>% 
#                  str_replace_all("[.]pdf$", ""))
#       ) 
#   
#   
#   
#   
#   
# 
# 
# 
# 
