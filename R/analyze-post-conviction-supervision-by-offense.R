rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(DBI)
library(dbplyr)
library(RPostgres)
library(scales)
library(forcats)


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
##Post-Sentence Conviction by Crime Type
sup_pop_by_crime_type <- 
  con %>% 
  tbl(DBI::Id("us_courts", "us_federal_courts_data")) %>% 
  collect() %>% 
  filter(table_num == "E-3") %>% 
  filter(table_name_date == max(table_name_date, na.rm = TRUE)) %>% 
  filter(state_or_district == "TOTAL") %>% 
  dplyr::select(variables_simple, values) %>% 
  mutate(perc = values / sum(values, na.rm = TRUE), 
         variables_simple = 
           variables_simple %>% 
           str_replace_all("_", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Escape Obstruction", "Escape/Obstruction") %>% 
           fct_reorder(values)) %>%
  arrange(desc(perc)) %>% 
  ggplot(aes(x = values, y = variables_simple, fill = perc)) + 
  geom_col() +
  geom_text(
    aes(label = percent(round(perc, 3))),
    hjust = -0.1
  ) +
  scale_x_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_gradient(
    low = "white",
    high = "forestgreen",
    # labels = scales::percent,
    name = NULL
  ) +
  labs(
    x = "Post-Conviction Supervision Population",
    y = "Offense Type",
    title = "Post Conviction Supervision Population by Offense Type - September 2024"
  ) +
  guides(fill = "none") +
  theme_minimal()


####Write Plot####
##PDF and PNG
ggsave(here("plots", "sup_pop_by_crime_type.pdf"), plot = sup_pop_by_crime_type, width = 15, height = 9)





