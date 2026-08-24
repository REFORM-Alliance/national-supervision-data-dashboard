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
library(readxl)
library(data.table)


####Prep for App####
##PPI Data
ppi_data <- 
  "data-raw" %>% 
  here("ppi") %>% 
  here("incarceration_supervision_by_state_1978_2024.xlsx") %>% 
  read_excel(sheet = "data") %>% 
  set_names(paste0("x", 1:ncol(.))) %>% 
  clean_names() %>% 
  mutate(keep = 
           ifelse(x1 == "state_name", 1, NA)) %>% 
  tidyr::fill(keep, .direction = "down") %>% 
  mutate(keep = 
           keep %>% 
           replace_na(0)) %>% 
  filter(keep == 1) %>% 
  dplyr::select(-keep) %>% 
  row_to_names(row_number = 1) %>% 
  mutate(control_type = 
           control_type %>% 
           str_to_title(), 
         across(c("count", "rate", "total_pop"), 
                ~.x %>% 
                  as.numeric())) %>% 
  filter(!(control_type == "TOtal Correctional Control"))

##BJS Data
bjs_data_raw <-
  "data" %>%
  here("bjs_data_raw.csv") %>%
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
  clean_names()

bjs_data_filtered <-
  bjs_data_raw %>%
  mutate(date_to_adjust =
           ifelse(is.na(date_to),
                  as.Date(paste0(year, "-01-01")),
                  as.Date(date_to))) %>%
  group_by(supervision_type, year, state, data_variables_clean) %>%
  filter(date_to_adjust == max(date_to_adjust, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(-date_to_adjust) %>%
  rename(value = data_values) %>%
  mutate(
    outcome_category = case_when(
      data_variables_clean == "population"                               ~ "Population",
      data_variables_clean == "population_per_100000_adult_residents"   ~ "Population (per 100k)",
      data_variables_clean %in% c("entries_reported", "entries_imputed") ~ "Admissions",
      data_variables_clean %in% c("exits_reported",   "exits_imputed")   ~ "Exits",
      TRUE ~ "Other"
    )
  )

##Calculate Georgia Data 
georgia_df <- 
  bjs_data_filtered %>% 
  filter(state == "Georgia") %>% 
  filter(data_variables_clean %in% c("entries_imputed", "exits_imputed")) %>% 
  filter(supervision_type %in% c("Parole", "Probation", "Community Supervision")) %>% 
  filter(str_detect(table_name_clean, "exiting") == FALSE) %>% 
  dplyr::select(state, year, supervision_type, data_variables_clean, value) %>% 
  mutate(data_variables_clean = 
           data_variables_clean %>% 
           str_replace_all("_imputed$", "")) %>% 
  pivot_wider(names_from = data_variables_clean, 
              values_from = value) %>% 
  left_join(ppi_data %>% 
              dplyr::select(year, state_name, control_type, count, total_pop) %>% 
              mutate(year = 
                       year %>% 
                       as.double()), 
            by = c("state" = "state_name", 
                   "year", 
                   "supervision_type" = "control_type")) %>% 
  rename("population" = "count", 
         "state_total_pop" = "total_pop") %>% 
  pivot_longer(cols = c("entries", "exits", "population"), 
               names_to = "measure_type", 
               values_to = "count") %>% 
  mutate(rate = 
           100000 * (count/state_total_pop)) %>% 
  pivot_longer(cols = c("count", "rate"), 
               names_to = "count_or_rate", 
               values_to = "value")


####Visualizations####
##Facet Grid
georgia_viz <- 
  georgia_df %>% 
  ggplot(aes(x = year, 
             y = value, 
             color = measure_type)) + 
  facet_grid(count_or_rate ~ supervision_type, scales = "free_y") + 
  geom_line() + 
  geom_point()
