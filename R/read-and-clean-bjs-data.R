rm(list = ls())


####Read in Libraries####
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(DBI)
library(RPostgres)
library(dbplyr)
library(readxl)
library(readr)


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


####Helper Functions####
##XLSX to CSV
xlsx_to_csv <- function(path, recursive = TRUE, overwrite = FALSE) {
  is_folder <- dir.exists(path)
  
  files <- c(
    unlist(lapply(path[is_folder], list.files, full.names = TRUE, recursive = recursive)),
    path[!is_folder]
  )
  files <- files[grepl("\\.xlsx?$", files, ignore.case = TRUE) &
                   !grepl("^~\\$", basename(files))]   # skip Excel's temporary lock files
  
  for (f in files) {
    out <- sub("\\.xlsx?$", ".csv", f, ignore.case = TRUE)
    
    if (file.exists(out) && !overwrite) {
      message("Skipped (CSV already exists): ", basename(out))
      next
    }
    
    tbl <- read_excel(f,
                      sheet = 1,
                      col_names = FALSE,
                      col_types = "text",
                      range = cell_limits(c(1, 1), c(NA, NA)),   # start at A1 so no blank rows are dropped
                      .name_repair = "minimal")
    
    write_csv(tbl, out, na = "", col_names = FALSE)
    message("Wrote ", out)
  }
  
  invisible(files)
}


####Read in and Clean Data####
##Convert XLSX to CSV
here("data-raw") %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("ppus") %>% 
  map(~.x %>% 
        list.files(full.names = TRUE)) %>% 
  unlist() %>% 
  map(~.x %>% 
        xlsx_to_csv())
  

##BJS Data
bjs_full_df <- 
  here("data-raw") %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("ppus") %>% 
  map(~.x %>% 
        list.files(full.names = TRUE)) %>% 
  unlist() %>% 
  str_subset(".csv$") %>% 
  map(~{
    file_raw <- .x
    
    raw <- 
      file_raw %>% 
      fread(header = FALSE, fill = TRUE) %>% 
      mutate(V1 = 
               V1 %>% 
               str_replace_all("Appendix Table", "Appendix table") %>% 
               na_if(""),
             note_flag = 
               V1 %>% 
               replace_na("NA"),
             note_flag = 
               ifelse(str_detect(note_flag, "^Note: ") == TRUE, 1, 0) %>% 
               cumsum()) %>% 
      filter(note_flag == 0) %>% 
      dplyr::select(-note_flag)
    
    first_appendix_row <- tail(which(str_detect(raw[[1]], regex("Appendix table", ignore_case = TRUE))), 1)
    jurisdiction_row <- which(str_detect(raw[[1]], regex("^Jurisdiction", ignore_case = TRUE)))[1]
    if(is.na(jurisdiction_row) == TRUE){
      jurisdiction_row <- which(str_detect(raw[[1]], regex("^Federal", ignore_case = TRUE)))[1] - 2
    }
    us_total_row <- which(str_detect(raw[[2]], regex("U.S. total", ignore_case = TRUE)))[1]
    
    if(us_total_row == jurisdiction_row){
      jurisdiction_row = jurisdiction_row - 1 
    }
    
    table_name <- 
      raw %>% 
      slice(first_appendix_row) %>% 
      pull(V1)
    
    shift_down_if_last_na <- function(col) {
      n <- length(col)
      if (is.na(col[n]) && any(!is.na(col[1:(n - 1)]))) {
        return(c(NA, col[1:(n - 1)]))
      } else {
        return(col)
      }
    }
    
    header_block <- 
      raw[(first_appendix_row + 1):(jurisdiction_row), ] %>% 
      mutate(across(where(is_character),
                    ~.x %>% 
                      na_if(""))) %>% 
      filter(!if_all(everything(), is.na)) %>% 
      mutate(across(everything(), shift_down_if_last_na)) %>% 
      filter(!if_all(everything(), is.na)) %>% 
      mutate(row_num = row_number(), 
             row_num = (max(row_num, na.rm = TRUE) + 1) - row_num,
             row_num = ifelse(row_num >= 2, 2, row_num),
             row_num = ifelse(row_num == 1, 2, 1)) %>% 
      group_by(row_num) %>% 
      dplyr::summarize(across(where(is_character), 
                              ~.x %>% 
                                replace_na("") %>% 
                                paste(collapse = " "))) %>% 
      ungroup() %>% 
      dplyr::select(-row_num) %>% 
      mutate(across(where(is_character),
                    ~.x %>% 
                      na_if("") %>% 
                      na_if(" "))) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(lag_v2 = 
               V2 %>% 
               lag(),
             V1 = 
               ifelse(is.na(V1) == TRUE & 
                        (str_detect(V2, "Absconder") == TRUE | str_detect(V2, "absconder") == TRUE) & 
                        (str_detect(lag_v2, "^other") == TRUE | str_detect(lag_v2, "^Other") == TRUE),
                      "Unsatisfactory Reason Other than Incarceration", 
                      V1)) %>% 
      dplyr::select(-lag_v2) %>% 
      tidyr::fill(V1, .direction = "down") %>% 
      mutate(V1 =
               ifelse(is.na(V2) == TRUE, NA, V1),
             V1 =
               ifelse(str_detect(V2, "Community supervision") == TRUE |
                        str_detect(V2, "community supervision") == TRUE | 
                        (str_detect(V2, "per 100,000") == TRUE & str_detect(V1, "^Number on") == FALSE),
                      NA, V1),
             across(where(is_character),
                    ~.x %>% 
                      str_replace_all("\x96", "")), 
             row_num = row_number(), 
             V1 = 
               ifelse(row_num == max(row_num, na.rm = TRUE) & 
                        is.na(V1) == TRUE & 
                        is.na(V2) == TRUE, 
                      lag(V1), 
                      V1),
             V2 = 
               ifelse(row_num == max(row_num, na.rm = TRUE) & 
                        is.na(V1) == TRUE & 
                        is.na(V2) == TRUE, 
                      lag(V2), 
                      V2)) %>% 
      dplyr::select(-row_num) %>% 
      as.matrix() %>% 
      t()
    
    combined_header <- 
      header_block %>%
      as_tibble(.name_repair = "unique") %>%
      mutate_all(as.character) %>%
      summarise(across(everything(), ~paste(na.omit(trimws(.x[.x != ""])), collapse = " - "))) %>%
      unlist() %>%
      as.character() %>% 
      str_replace_all("Jurisdiction", "") %>% 
      str_replace_all("U.S. total", "") %>% 
      str_remove(",[a-z]$") %>% 
      str_remove("/[a-z]$")
    
    og <- 
      file_raw %>% 
      fread(sep = ",", stringsAsFactors = FALSE, skip = jurisdiction_row, header = FALSE) %>% 
      mutate(across(where(is_character),
                    ~.x %>%
                      na_if("")),
             note_flag = 
               V1 %>% 
               replace_na("NA"),
             note_flag = 
               ifelse(str_detect(note_flag, "^Note: ") == TRUE, 1, 0) %>% 
               cumsum()) %>% 
      filter(note_flag == 0) %>% 
      dplyr::select(-note_flag) %>%
      select(where(~ !all(is.na(.)))) %>% 
      setNames(combined_header[1:ncol(.)]) %>% 
      clean_names() %>% 
      rename_with(~.x %>% 
                    str_remove("_[a-zA-Z]$") %>% 
                    str_replace_all("_[a-zA-Z]_", "_") %>% 
                    str_replace_all("reported_2", "reported_exits") %>% 
                    str_replace_all("imputed_2", "imputed_exits") %>% 
                    str_replace_all("^x(_\\d+)?$", "state\\1") %>% 
                    str_replace_all("^reported$", "reported_entries") %>% 
                    str_replace_all("^imputed$", "imputed_entries")) %>% 
      mutate(across(where(is_character),
                    ~.x %>%
                      na_if("")),
             note_flag = 
               state %>% 
               replace_na("NA"),
             note_flag = 
               ifelse(str_detect(note_flag, "^Note: ") == TRUE, 1, 0) %>% 
               cumsum()) %>% 
      filter(note_flag == 0) %>% 
      dplyr::select(-note_flag) %>%
      mutate(across(where(is_character),
                    ~.x %>% 
                      na_if("") %>% 
                      na_if("%") %>% 
                      na_if("**") %>% 
                      na_if("--") %>% 
                      na_if("/") %>% 
                      na_if(":") %>% 
                      na_if("..") %>% 
                      na_if("~") %>% 
                      na_if("...") %>% 
                      str_replace_all("\x86", "")),
             state = 
               state %>% 
               na_if("Jurisdiction"),
             state =
               ifelse(is.na(state) == TRUE, state_2, state),
             state_2 = ifelse(state == state_2, NA, state_2)) %>%
      select(where(~ !all(is.na(.)))) %>% 
      separate_wider_delim(cols = state, 
                           delim = "/",
                           names = c("state", "state_extra"),
                           too_few = "align_start", 
                           too_many = "drop") %>% 
      dplyr::select(-state_extra) %>% 
      mutate(state = 
               state %>% 
               str_replace_all("State", "State Total") %>% 
               str_replace_all("total", "Total"),
             across(where(is_character), 
                    ~.x %>% 
                      str_replace_all(",", "") %>% 
                      str_replace_all(":", "")),
             across(where(is_character),
                    ~ifelse(str_detect(.x, "^<") == TRUE | str_detect(.x, "^>") == TRUE, NA, .x)),
             across(-c("state"), 
                    ~.x %>% 
                      as.numeric()),
             table_name = 
               table_name %>% 
               str_replace_all(":", ".")) %>% 
      separate_wider_delim(cols = table_name,
                           delim = ".",
                           names = c("table_num", "table_name")) %>% 
      mutate(table_name = 
               table_name %>% 
               str_replace_all(",([^,]*)$", "|||\\1")) %>% 
      separate_wider_delim(table_name, 
                           delim = "|||", 
                           names = c("table_name", "year")) %>% 
      mutate(across(where(is_character),
                    ~.x %>% 
                      trimws(which = "both")),
             year = 
               year %>% 
               as.numeric())
  }) %>% 
  map(~.x %>% 
        pivot_longer(cols = -c(state, year, starts_with("table")), 
                     names_to = "data_variables",
                     values_to = "data_values")) %>% 
  bind_rows() %>% 
  mutate(table_name = 
           table_name %>% 
           str_replace_all("Selected", "Select") %>% 
           str_replace_all("Select", "Selected")) %>% 
  separate_wider_delim(cols = "table_name", 
                       delim = ",",
                       names = c("table_name_clean", "table_name_clean_2"), 
                       cols_remove = FALSE, 
                       too_few = "align_start") %>% 
  dplyr::select(-table_name_clean_2) %>% 
  mutate(supervision_type = 
           case_when(str_detect(table_name_clean, "parole") == TRUE ~ "Parole",
                     str_detect(table_name_clean, "felony probation") == TRUE ~ "Felony Probation",
                     str_detect(table_name_clean, "misdemeanor probation") == TRUE ~ "Misdemeanor Probation",
                     str_detect(table_name_clean, "felony probation") == FALSE & 
                       str_detect(table_name_clean, "misdemeanor probation") == FALSE & 
                       str_detect(table_name_clean, "probation") == TRUE ~ "Probation",
                     str_detect(table_name_clean, "community supervision") == TRUE ~ "Community Supervision",
                     TRUE ~ NA), 
         data_variables = 
           data_variables %>% 
           str_replace_all("community_supervision_population_12_31_2012a", "community_supervision_population_12_31_2012"),
         date = 
           data_variables %>% 
           str_replace_all("january", "1") %>% 
           str_replace_all("december", "12") %>% 
           str_extract_all("\\d{1,2}_\\d{1,2}_\\d{4}") %>% 
           map(~.x %>% 
                 str_replace_all("_", "/") %>% 
                 mdy() %>% 
                 unique()),
         data_variables_clean = 
           data_variables %>% 
           str_replace_all("january", "1") %>% 
           str_replace_all("december", "12") %>% 
           str_remove_all("\\d{1,2}_\\d{1,2}_\\d{4}") %>% 
           str_remove("_$") %>% 
           str_trim() %>% 
           str_replace_all("^change_\\d+_(percent|number)$", "change_\\1") %>% 
           str_replace_all("^_number$", "change_number") %>% 
           str_replace_all("^_percent$", "change_percent") %>% 
           str_remove("^_") %>% 
           str_replace_all("100_000", "_100000_") %>% 
           str_replace_all("adult_s", "adults") %>% 
           str_replace_all("s_adult", "adults") %>% 
           str_replace_all("_under_", "_") %>% 
           str_replace_all("_adults_", "_adult_") %>%
           str_replace_all("_\\d$", "") %>% 
           str_replace_all("adult_residents_", "adult_residents") %>% 
           str_replace_all("number_probation", "number_on_probation") %>%
           str_replace_all("^total$", "total_reported") %>% 
           str_replace_all("_$", "") %>% 
           str_replace_all("^number_on_parole", "parole_population") %>% 
           str_replace_all("^number_on_probation", "probation_population") %>% 
           str_replace_all("^number_community_supervision", "community_supervision_population") %>% 
           str_replace_all("^incarcerated_discharged_to_warrant_detainer$", "incarcerated_discharged_to_warrant_or_detainer") %>% 
           str_replace_all("^returned_to_incarceration_", "incarcerated_") %>% 
           str_replace_all("parole_population", "population") %>% 
           str_replace_all("probation_population", "population") %>% 
           str_replace_all("community_supervision_population", "population") %>% 
           str_replace_all("exits_population", "population") %>% 
           str_replace_all("_estimated$", "_imputed") %>%
           str_replace_all("^unsatisfactory_exit_other_than_returned_to_incarceration_", "unsatisfactory_reason_other_than_incarceration_") %>% 
           str_replace_all("^unsatisfactory_reason_other_than_returned_to_incarceration_", "unsatisfactory_reason_other_than_incarceration_") %>% 
           str_replace_all("^unsatisfactory_other_than_incarceration_", "unsatisfactory_reason_other_than_incarceration_") %>% 
           str_replace_all("unknown_not_reported$", "unknown_or_not_reported") %>%
           str_replace_all("unknown_or_not_reported$", "other_unknown_or_not_reported") %>%
           str_replace_all("^total_completion$", "completion") %>%
           str_replace_all("^total_reported$", "exits_reported") %>%
           str_replace_all("discharged_to_warrant_detainer$", "discharged_to_warrant_or_detainer") %>% 
           str_replace_all("discharged_to_warrant_or_retainer$", "discharged_to_warrant_or_detainer") %>%
           str_replace_all("_not_asked$", "_other_not_asked") %>% 
           str_replace_all("^unsatisfactory_reason_other_than_incarceration_unknown$", "unsatisfactory_reason_other_than_incarceration_other_unknown") %>%
           str_replace_all("_{2,}", "_"),
         row_num = 1:n()) %>% 
  unnest_longer(date, 
                keep_empty = TRUE) %>% 
  arrange(row_num, date) %>% 
  distinct() %>% 
  group_by(row_num) %>% 
  mutate(date_num = 1:n()) %>% 
  ungroup() %>% 
  mutate(date_cat = 
           case_when(date_num == 1 ~ "date_to", 
                     date_num == 2 ~ "date_from", 
                     TRUE ~ NA)) %>% 
  dplyr::select(-date_num) %>% 
  pivot_wider(names_from = "date_cat",
              values_from = "date") %>% 
  dplyr::select(-c(row_num, table_name, data_variables)) %>% 
  mutate(state = 
           state %>% 
           str_replace_all("[*]$", "") %>% 
           na_if("..Not known.") %>% 
           na_if("~Not applicable.") %>% 
           na_if("Some or all data are estimates.") %>% 
           na_if("Source Bureau of Justice Statistics Annual Parole Survey 2015.") %>% 
           na_if("*Some or all data were estimates.") %>% 
           na_if("Source Bureau of Justice Statistics Annual Parole Survey 2016.") %>% 
           na_if("a") %>% 
           na_if("b") %>% 
           na_if("c") %>% 
           str_replace_all("District Of Columbia", "District of Columbia")) %>% 
  filter(is.na(state) == FALSE) %>% 
  mutate(incarcerated_flag = ifelse(str_detect(data_variables_clean, "^incarcerated_") == TRUE, 1, 0)) %>% 
  group_by(state, table_num, table_name_clean, year, supervision_type, incarcerated_flag, date_to, date_from) %>% 
  mutate(blahblah_incarcerated_total = 
           data_values %>% 
           sum(na.rm = TRUE) %>% 
           replace_na(99999)) %>% 
  ungroup() %>% 
  mutate(blahblah_incarcerated_total = 
           ifelse(incarcerated_flag == 1, blahblah_incarcerated_total, NA)) %>% 
  dplyr::select(-incarcerated_flag) %>% 
  mutate(data_variables_clean = 
           paste0("blahblah_", data_variables_clean),
         data_values = 
           data_values %>% 
           replace_na(99999)) %>% 
  pivot_wider(names_from = "data_variables_clean",
              values_from = "data_values") %>% 
  pivot_longer(cols = starts_with("blahblah_"),
               names_to = "data_variables_clean", 
               values_to = "data_values") %>% 
  mutate(data_variables_clean = 
           data_variables_clean %>% 
           str_replace_all("^blahblah_", "")) %>% 
  filter(is.na(data_values) == FALSE,
         !(data_variables_clean == "incarcerated_total" & is.na(date_to) == FALSE)) %>%
  mutate(data_values = 
           data_values %>% 
           na_if(99999)) %>% 
  mutate(unsat_flag = ifelse(str_detect(data_variables_clean, "^unsatisfactory_reason_other_than_incarceration_") == TRUE, 1, 0)) %>% 
  group_by(state, table_num, table_name_clean, year, supervision_type, unsat_flag, date_to, date_from) %>% 
  mutate(blahblah_unsatisfactory_reason_other_than_incarceration_total = 
           data_values %>% 
           sum(na.rm = TRUE) %>% 
           replace_na(99999)) %>% 
  ungroup() %>% 
  mutate(blahblah_unsatisfactory_reason_other_than_incarceration_total = 
           ifelse(unsat_flag == 1, blahblah_unsatisfactory_reason_other_than_incarceration_total, NA)) %>% 
  dplyr::select(-unsat_flag) %>% 
  mutate(data_variables_clean = 
           paste0("blahblah_", data_variables_clean),
         data_values = 
           data_values %>% 
           replace_na(99999)) %>% 
  pivot_wider(names_from = "data_variables_clean",
              values_from = "data_values") %>% 
  pivot_longer(cols = starts_with("blahblah_"),
               names_to = "data_variables_clean", 
               values_to = "data_values") %>% 
  mutate(data_variables_clean = 
           data_variables_clean %>% 
           str_replace_all("^blahblah_", "")) %>% 
  filter(is.na(data_values) == FALSE,
         !(data_variables_clean == "unsatisfactory_reason_other_than_incarceration_total" & is.na(date_to) == FALSE)) %>%
  mutate(data_values = 
           data_values %>% 
           na_if(99999), 
         data_variables_title = 
           data_variables_clean %>% 
           str_replace_all("_", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Entries", "Entries -") %>% 
           str_replace_all("Exits", "Exits -") %>% 
           str_replace_all("Change", "Change -") %>% 
           str_replace_all("Incarcerated", "Incarcerated -") %>% 
           str_replace_all("Unsatisfactory Reason Other Than Incarceration", "Unsatisfactory Reason Other Than Incarceration -") %>% 
           str_replace_all("Sex", "Sex -") %>% 
           str_replace_all("Race Ethnicity", "Race/Ethnicity -") %>% 
           str_replace_all(" - Other", " - Other - Other") %>% 
           str_replace_all(" - Other - Other", " - Other ;;;")) %>% 
  separate_wider_delim(data_variables_title, 
                       delim = " ;;; ", 
                       names = c("data_variables_title_base", 
                                 "data_variables_title_2"), 
                       too_few = "align_start") %>% 
  mutate(data_variables_title_2 = 
           ifelse(is.na(data_variables_title_2) == FALSE,
                  paste0("(", data_variables_title_2, ")"), 
                  NA),
         data_variables_title = 
           ifelse(is.na(data_variables_title_2) == FALSE,
                  data_variables_title_base %>% 
                    paste(data_variables_title_2, sep = " "), 
                  data_variables_title_base) %>% 
           str_replace_all(" ;;;$", "")) %>% 
  dplyr::select(-c(data_variables_title_2, data_variables_title_base)) %>% 
  relocate(data_variables_title, .after = "data_variables_clean")


####Write Data####
##BJS Data to DB
dbWriteTable(con, "full_bjs_data", bjs_full_df, overwrite = TRUE)

##BJS Data to CSV
fwrite(bjs_full_df, here("data", "bjs_data_raw.csv"), row.names = FALSE)

##Write CSV of 2023 Supervised Populations by State
community_supervision_pop_by_state <- 
  bjs_full_df %>% 
  mutate(date_month = month(date_to)) %>% 
  filter(data_variables_clean == "population",
         supervision_type == "Community Supervision",
         date_month == 12,
         year == max(year, na.rm = TRUE)) %>% 
  dplyr::select(state, year, data_variables_clean, data_values) %>% 
  distinct() %>% 
  mutate(sum_groups = ifelse(state %in% c("U.S. Total", "Federal", "State Total"), 1, 0)) %>% 
  pivot_wider(names_from = "data_variables_clean",
              values_from = "data_values") %>% 
  group_by(sum_groups) %>% 
  mutate(state_rank = min_rank(desc(population))) %>% 
  ungroup() %>% 
  rename("fiscal_year" = "year") %>% 
  mutate(state_rank = ifelse(sum_groups == 1, 0, state_rank)) %>% 
  arrange(state_rank, desc(population)) %>% 
  dplyr::select(-sum_groups) %>% 
  mutate(state_rank = 
           state_rank %>% 
           na_if(0))

fwrite(community_supervision_pop_by_state, here("data", "community_supervision_pop_by_state.csv"), row.names = FALSE)

# blah <- 
#   community_supervision_pop_by_state %>% 
#   filter(state %in% c("Alabama", 
#                       "Arizona", 
#                       "California",
#                       "Colorado", 
#                       "Connecticut", 
#                       "Delaware",
#                       "Florida", 
#                       "Georgia", 
#                       "Hawaii", 
#                       "Illinois", 
#                       "Iowa", 
#                       "Kansas", 
#                       "Kentucky", 
#                       "Louisiana", 
#                       "Maryland", 
#                       "Minnesota", 
#                       "Missouri", 
#                       "North Carolina", 
#                       "New Jersey", 
#                       "New York", 
#                       "Ohio", 
#                       "Pennsylvania", 
#                       "Rhode Island", 
#                       "South Carolina", 
#                       "Tennessee", 
#                       "Texas", 
#                       "Utah", 
#                       "Vermont", 
#                       "Virginia", 
#                       "Washington", 
#                       "Wisconsin", 
#                       "Wyoming")) %>% 
#   mutate(pop_n_rank = 
#            paste0(as.character(comma_format()(community_supervision_population)), " (", as.character(state_rank), ")")) %>% 
#   arrange(state)
# 
# fwrite(blah, here("data", "blah.csv"), row.names = FALSE)
