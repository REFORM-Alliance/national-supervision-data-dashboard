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


####Read in BJS Data####
##Full BJS Data
full_bjs_women <- 
  con %>% 
  tbl("full_bjs_data") %>% 
  # filter(str_detect(data_variables_clean, "^sex") == TRUE) %>% 
  filter(str_detect(table_name_clean, "^Selected characteristics") == TRUE) %>% 
  collect() %>% 
  separate_wider_delim(data_variables_title, delim = " - ", names = c("var_type", "var_option"), too_few = "align_start") %>% 
  filter(var_type == "Sex") %>% 
  dplyr::select(state, year, supervision_type, var_option, data_values) %>% 
  pivot_wider(names_from = "var_option", 
              values_from = "data_values") %>% 
  clean_names() %>% 
  filter(year == 2023) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0)),
         female_perc = 
           female/(male + female), 
         female_perc_master = 
           ifelse(state == "U.S. Total", female_perc, NA)) %>% 
  group_by(year, supervision_type) %>% 
  tidyr::fill(female_perc_master, .direction = "downup") %>% 
  mutate(female_perc = 
           ifelse(is.na(female_perc) == TRUE, female_perc_master, female_perc)) %>% 
  ungroup() %>% 
  mutate(unknown_not_reported = 
           other_unknown_or_not_reported + other_not_asked, 
         female_imputed = 
           female + (female_perc * unknown_not_reported)) %>% 
  filter(!(state %in% c("U.S. Total", "State Total"))) %>% 
  group_by(supervision_type) %>% 
  dplyr::summarize(across(c("female_imputed", "female"), 
                          ~.x %>% 
                            replace_na(0) %>% 
                            sum(na.rm = TRUE))) %>% 
  ungroup()

# full_bjs_women2 <- 
#   con %>% 
#   tbl("full_bjs_data") %>% 
#   # filter(str_detect(data_variables_clean, "^sex") == TRUE) %>% 
#   filter(str_detect(table_name_clean, "^Selected characteristics") == TRUE) %>% 
#   collect() %>% 
#   separate_wider_delim(data_variables_title, delim = " - ", names = c("var_type", "var_option"), too_few = "align_start") %>% 
#   filter(var_type == "Sex") %>% 
#   dplyr::select(state, year, supervision_type, var_option, data_values) %>% 
#   pivot_wider(names_from = "var_option", 
#               values_from = "data_values") %>% 
#   clean_names() %>% 
#   filter(year == 2023) %>% 
#   mutate(across(where(is.numeric), 
#                 ~.x %>% 
#                   replace_na(0)),
#          female_perc = 
#            female/(male + female), 
#          unknown_not_reported = 
#            other_unknown_or_not_reported + other_not_asked, 
#          female_imputed = 
#            female + (female_perc * unknown_not_reported)) %>% 
#   filter(state %in% c("U.S. Total", "State Total", "Federal")) %>% 
#   group_by(supervision_type, state) %>% 
#   dplyr::summarize(across(c("female_imputed", "female"), 
#                           ~.x %>% 
#                             replace_na(0) %>% 
#                             sum(na.rm = TRUE))) %>% 
#   ungroup()

##Last Year vs. First Year of Data
last_v_first_women_df <- 
  con %>% 
  tbl("full_bjs_data") %>% 
  # filter(str_detect(data_variables_clean, "^sex") == TRUE) %>% 
  collect() %>% 
  filter(str_detect(table_name_clean, "^Selected characteristics") == TRUE) %>% 
  separate_wider_delim(data_variables_title, delim = " - ", names = c("var_type", "var_option"), too_few = "align_start") %>% 
  filter(var_type == "Sex") %>% 
  dplyr::select(state, year, supervision_type, var_option, data_values) %>% 
  pivot_wider(names_from = "var_option", 
              values_from = "data_values") %>% 
  clean_names() %>% 
  filter(year == 2023) %>% 
  bind_rows(con %>% 
              tbl(DBI::Id("bjs", "bjs_parole_characteristics_data")) %>% 
              filter(measure == "Sex") %>% 
              mutate(supervision_type = "Parole") %>% 
              union_all(con %>% 
                          tbl(DBI::Id("bjs", "bjs_probation_characteristics_data")) %>% 
                          filter(measure == "Sex") %>% 
                          mutate(supervision_type = "Probation")) %>% 
              mutate(year = 
                       year %>% 
                       as.numeric()) %>%
              group_by(jurisdiction, supervision_type) %>% 
              filter(year == min(year, na.rm = TRUE)) %>% 
              ungroup() %>% 
              rename("state" = "jurisdiction") %>% 
              dplyr::select(-measure) %>% 
              pivot_wider(names_from = "variable", 
                          values_from = "values") %>% 
              collect() %>% 
              clean_names() %>% 
              rename("other_unknown_or_not_reported" = "unknown") %>% 
              mutate(state = 
                       case_when(state == "National statistics (U.S. total)" ~ "U.S. Total", 
                                 state == "State (total)" ~ "State Total", 
                                 TRUE ~ state))) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0)),
         female_perc = 
           female/(male + female), 
         female_perc_master = 
           ifelse(state == "U.S. Total", female_perc, NA)) %>% 
  group_by(year, supervision_type) %>% 
  tidyr::fill(female_perc_master, .direction = "downup") %>% 
  mutate(female_perc = 
           ifelse(is.na(female_perc) == TRUE, female_perc_master, female_perc)) %>% 
  ungroup() %>% 
  mutate(unknown_not_reported = 
           other_unknown_or_not_reported + other_not_asked, 
         female_imputed = 
           female + (female_perc * unknown_not_reported)) %>% 
  filter(!(state %in% c("U.S. Total", "State Total")))
  
last_v_first_women_total_df <- 
  last_v_first_women_df %>% 
  group_by(supervision_type, year) %>% 
  dplyr::summarize(across(c("female_imputed", "female"), 
                          ~.x %>% 
                            replace_na(0) %>% 
                            sum(na.rm = TRUE))) %>% 
  ungroup()
  
  
  
  