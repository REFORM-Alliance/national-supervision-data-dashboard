rm(list = ls())


####Read in Libraries####
library(tidyverse)
library(data.table)
library(janitor)
library(RSelenium)
library(magick)
library(base64enc)
library(curl)
library(processx)
library(httr2)
library(glue)
library(rvest)
library(here)
library(DBI)
library(RPostgres)
library(dbplyr)


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

##Kill Selenium
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


###################################
############ PROBATION ############
###################################


####Scrape Probation Dashboards####
##Set up Download Directory
download_dir <- 
  "data-raw" %>% 
  here("probation-demographic-data")

if(dir.exists(download_dir) == FALSE){
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

chrome_prefs <- list(
  "download.default_directory" = download_dir,
  "download.prompt_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing.enabled" = TRUE, 
  "intl.charset_default" = "UTF-8"
)

##Start Selenium
free_port = find_free_port(sudo_password = getOption("sudo_password"))
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
      args = chrome_args, 
      prefs = chrome_prefs
    )
  )
)


tryCatch(
  invisible(capture.output(remDr$open(), type = "output")),
  error = function(e) print(e)
)

if(exists("remDr")){
  remDr$navigate("https://charts.ojp.usdoj.gov/t/public/views/Probation-Dashboard/Characteristics?%3Aembed=y&%3AisGuestRedirectFromVizportal=y")
  page_title <- remDr$getTitle()
}else{
  cat("Failed to open the remote browser session. Check for errors.\n")
}

Sys.sleep(10)

search_by_measure <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_1"]/div/div[3]')
search_by_measure$clickElement()
Sys.sleep(2)

# remDr$screenshot(file = "measure_opened_first.png")

# Get options
measure_texts <- 
  remDr$findElements(using = "xpath", value = "//div[@role='option']") %>% 
  sapply(
    function(x){
      tryCatch({
        a_elem <- x$findElement(using = "tag name", value = "a")
        a_elem$getElementText()[[1]]
      }, error = function(e) {
        x$getElementText()[[1]]
      })
    }) %>% 
  str_subset("Total probation population", negate = TRUE)

Sys.sleep(10)

remDr$refresh()

Sys.sleep(3)

search_by_year <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_0"]/div/div[3]')
search_by_year$clickElement()

Sys.sleep(3)

# Get all unchecked year checkbox inputs
unchecked_checkboxes <- remDr$findElements(using = "xpath", 
                                           value = "//div[@role='checkbox' and @aria-checked='false']//input[@type='checkbox']")

Sys.sleep(3)
cat("Found", length(unchecked_checkboxes), "unchecked checkboxes\n")

# Click each checkbox directly with a good wait time
for(i in 1:length(unchecked_checkboxes)) {
  cat("Clicking checkbox", i, "\n")
  unchecked_checkboxes[[i]]$clickElement()
  Sys.sleep(0.5)  # Wait for Tableau to register the click
}

cat("\nDone clicking. Waiting for updates...\n")
Sys.sleep(2)

# Verify
final_unchecked <- remDr$findElements(using = "xpath", 
                                      value = "//div[@role='checkbox' and @aria-checked='false']//input[@type='checkbox']")
cat("Final unchecked:", length(final_unchecked), "\n")

checked <- remDr$findElements(using = "xpath", 
                              value = "//div[@role='checkbox' and @aria-checked='true']//input[@type='checkbox']")
cat("Checked:", length(checked), "\n")

search_by_year_button <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_0"]/div/div[3]/span')
search_by_year$click()

Sys.sleep(5)

search_by_year_button$click()

Sys.sleep(5)

final_df <- 
  measure_texts %>% 
  map(~{
    hooplah <- .x
    
    print(hooplah)
    
    "data-raw" %>% 
      here("probation-demographic-data") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset("Jurisdiction values.csv") %>% 
      map(~.x %>% 
            unlink(recursive = FALSE))
    
    file_name_output <- 
      "data-raw" %>% 
      here("probation-demographic-data") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(hooplah %>% 
                   str_to_lower() %>% 
                   str_replace_all(" ", "_") %>% 
                   str_replace_all("[(]", "") %>% 
                   str_replace_all("[)]", "") %>% 
                   str_replace_all("/", "_"))
    
    if(length(file_name_output) == 0){
      search_by_measure <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_1"]/div/div[3]')
      search_by_measure$clickElement()
      
      Sys.sleep(3)
      search_box <- remDr$findElement(using = "xpath", 
                                      value = "//textarea[contains(@class, 'QueryBox')]")
      search_box$clearElement()
      search_box$sendKeysToElement(list(hooplah))
      Sys.sleep(3)
      
      search_box$sendKeysToElement(list(key = "down_arrow"))
      Sys.sleep(1)
      search_box$sendKeysToElement(list(key = "return"))
      
      Sys.sleep(3)
      
      filtered_option <- remDr$findElement(using = "xpath", value = "//div[@role='option']//a")
      filtered_option$clickElement()
      filtered_option$click()
      
      Sys.sleep(2)
      
      download_button <- remDr$findElement(using = "xpath", value = '//*[@id="download"]')
      download_button$clickElement()
      download_button$click()
      
      Sys.sleep(2)
      
      suppressMessages(suppressWarnings({
        tryCatch({
          crosstab_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="viz-viewer-toolbar-download-menu"]/div[3]')
          crosstab_download_button$clickElement()
        }, error = function(e){
          Sys.sleep(2)
          download_button <- remDr$findElement(using = "xpath", value = '//*[@id="download"]')
          download_button$clickElement()
          crosstab_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="viz-viewer-toolbar-download-menu"]/div[3]')
          crosstab_download_button$clickElement()
        })
      }))
      
      Sys.sleep(2)
      
      csv_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[2]/div[2]/label[2]')
      csv_download_button$clickElement()
      
      Sys.sleep(2)
      
      jurisdiction_values_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[1]/div[2]/div/div/div[4]')
      jurisdiction_values_button$clickElement()
      
      Sys.sleep(2)
      
      final_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[3]/button')
      final_download_button$clickElement()
      
      Sys.sleep(2)
      
      download_elements <- remDr$findElements(
        using = "xpath",
        value = "//*[contains(text(), 'Download')]"
      )
      
      # Filter for just the "A" tag
      final_download_link <- NULL
      for(elem in download_elements) {
        tag <- elem$getElementAttribute("tagName")[[1]]
        if(tag == "A") {
          final_download_link <- elem
          break
        }
      }
      
      Sys.sleep(2)
      
      print("About to Download")
      
      tryCatch({
        final_download_link$clickElement()
        final_download_link$click()
      }, error = function(e) {
        message("Error clicking download link: ", e$message)
      })
      
      Sys.sleep(15)
      
      print("Download Complete")
      
      # Define file paths
      input_file <- 
        "data-raw" %>% 
        here("probation-demographic-data") %>% 
        list.files(full.names = TRUE) %>% 
        str_subset("Jurisdiction values.csv$")
      
      output_file <- 
        "data-raw" %>% 
        here("probation-demographic-data") %>% 
        paste0("/jurisdiction_data_",
               hooplah %>% 
                 str_to_lower() %>% 
                 str_replace_all(" ", "_") %>% 
                 str_replace_all("[(]", "") %>% 
                 str_replace_all("[)]", "") %>% 
                 str_replace_all("/", "_"), 
               ".csv")
      
      Sys.sleep(2)
      
      # Read as UTF-16 and write as UTF-8
      con_in <- file(input_file, encoding = "UTF-16")
      content <- readLines(con_in, warn = FALSE)
      close(con_in)
      
      Sys.sleep(1)
      
      con_out <- file(output_file, encoding = "UTF-8")
      writeLines(content, con_out)
      close(con_out)
      
      Sys.sleep(1)
      
      print("UTF8 tranformation complete")
      
      unlink(input_file, recursive = FALSE)
    }
    
    output_file <- 
      "data-raw" %>% 
      here("probation-demographic-data") %>% 
      paste0("/jurisdiction_data_",
             hooplah %>% 
               str_to_lower() %>% 
               str_replace_all(" ", "_") %>% 
               str_replace_all("[(]", "") %>% 
               str_replace_all("[)]", "") %>% 
               str_replace_all("/", "_"), 
             ".csv")
    
    # Now use fread on the new file
    dt <- 
      output_file %>% 
      fread(header = TRUE, stringsAsFactors = FALSE) %>% 
      clean_names() %>% 
      dplyr::select(jurisdiction, measure, variable, starts_with("x")) %>% 
      pivot_longer(cols = starts_with("x"), 
                   names_to = "year", 
                   values_to = "values") %>% 
      mutate(year = 
               year %>% 
               str_replace_all("^x", ""), 
             values = 
               values %>% 
               str_trim(side = "both") %>% 
               na_if("") %>% 
               na_if("##") %>% 
               na_if("~") %>% 
               na_if("/") %>%
               str_replace_all(",", "") %>% 
               str_replace_all("^-", "") %>% 
               as.numeric()) %>% 
      filter(is.na(values) == FALSE)
    
    Sys.sleep(10)
    
    return(dt)
  }) %>% 
  bind_rows()

##Kill Selenium
kill_selenium(port_to_kill = free_port)
Sys.sleep(10)


####Write Data####
##Create Schema
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bjs;")

##Insert into Database
dbWriteTable(con, DBI::Id("bjs", "bjs_probation_characteristics_data"), final_df, overwrite = TRUE)

##Write Compressed CSV
fwrite(final_df, here("data", "bjs_probation_characteristics_data.csv.gz"), compress = "gzip")


################################
############ PAROLE ############
################################


####Scrape Parole Dashboard####
##Set up Download Directory
download_dir <- 
  "data-raw" %>% 
  here("parole-demographic-data")

if(dir.exists(download_dir) == FALSE){
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

chrome_prefs <- list(
  "download.default_directory" = download_dir,
  "download.prompt_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing.enabled" = TRUE, 
  "intl.charset_default" = "UTF-8"
)

##Start Selenium
free_port = find_free_port(sudo_password = getOption("sudo_password"))
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
      args = chrome_args, 
      prefs = chrome_prefs
    )
  )
)


tryCatch(
  invisible(capture.output(remDr$open(), type = "output")),
  error = function(e) print(e)
)

if(exists("remDr")){
  remDr$navigate("https://charts.ojp.usdoj.gov/t/public/views/ParoleDashboard/Characteristics?%3Aembed=y&%3AisGuestRedirectFromVizportal=y")
  page_title <- remDr$getTitle()
}else{
  cat("Failed to open the remote browser session. Check for errors.\n")
}

Sys.sleep(10)

search_by_measure <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_1"]/div/div[3]')
search_by_measure$clickElement()
Sys.sleep(2)

# Get options
measure_texts <- 
  remDr$findElements(using = "xpath", value = "//div[@role='option']") %>% 
  sapply(
    function(x){
      tryCatch({
        a_elem <- x$findElement(using = "tag name", value = "a")
        a_elem$getElementText()[[1]]
      }, error = function(e) {
        x$getElementText()[[1]]
      })
    }) %>% 
  str_subset("Total parole population", negate = TRUE)

Sys.sleep(10)

remDr$refresh()

Sys.sleep(3)

search_by_year <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_0"]/div/div[3]')
search_by_year$clickElement()

Sys.sleep(3)

# Get all unchecked year checkbox inputs
unchecked_checkboxes <- remDr$findElements(using = "xpath", 
                                           value = "//div[@role='checkbox' and @aria-checked='false']//input[@type='checkbox']")

Sys.sleep(3)
cat("Found", length(unchecked_checkboxes), "unchecked checkboxes\n")

# Click each checkbox directly with a good wait time
for(i in 1:length(unchecked_checkboxes)) {
  cat("Clicking checkbox", i, "\n")
  unchecked_checkboxes[[i]]$clickElement()
  Sys.sleep(0.5)  # Wait for Tableau to register the click
}

cat("\nDone clicking. Waiting for updates...\n")
Sys.sleep(2)

# Verify
final_unchecked <- remDr$findElements(using = "xpath", 
                                      value = "//div[@role='checkbox' and @aria-checked='false']//input[@type='checkbox']")
cat("Final unchecked:", length(final_unchecked), "\n")

checked <- remDr$findElements(using = "xpath", 
                              value = "//div[@role='checkbox' and @aria-checked='true']//input[@type='checkbox']")
cat("Checked:", length(checked), "\n")

search_by_year_button <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_0"]/div/div[3]/span')
search_by_year$clickElement()

Sys.sleep(5)

search_by_year_button$click()
# remDr$screenshot(file = "blah.png")

Sys.sleep(5)

final_df <- 
  measure_texts %>% 
  map(~{
    hooplah <- .x
    
    print(hooplah)
    
    "data-raw" %>% 
      here("parole-demographic-data") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset("Jurisdiction values.csv") %>% 
      map(~.x %>% 
            unlink(recursive = FALSE))
    
    file_name_output <- 
      "data-raw" %>% 
      here("parole-demographic-data") %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(hooplah %>% 
                   str_to_lower() %>% 
                   str_replace_all(" ", "_") %>% 
                   str_replace_all("[(]", "") %>% 
                   str_replace_all("[)]", "") %>% 
                   str_replace_all("/", "_"))
    
    if(length(file_name_output) == 0){
      search_by_measure <- remDr$findElement(using = "xpath", value = '//*[@id="tableau_base_widget_LegacyCategoricalQuickFilter_1"]/div/div[3]')
      search_by_measure$clickElement()
      
      Sys.sleep(5)
      search_box <- remDr$findElement(using = "xpath", 
                                      value = "//textarea[contains(@class, 'QueryBox')]")
      search_box$clearElement()
      search_box$sendKeysToElement(list(hooplah))
      Sys.sleep(3)
      
      search_box$sendKeysToElement(list(key = "down_arrow"))
      Sys.sleep(1)
      search_box$sendKeysToElement(list(key = "return"))
      
      Sys.sleep(3)
      
      filtered_option <- remDr$findElement(using = "xpath", value = "//div[@role='option']//a")
      filtered_option$clickElement()
      filtered_option$click()
      
      Sys.sleep(2)
      
      download_button <- remDr$findElement(using = "xpath", value = '//*[@id="download"]')
      download_button$clickElement()
      download_button$click()
      
      Sys.sleep(2)
      
      suppressMessages(suppressWarnings({
        tryCatch({
          crosstab_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="viz-viewer-toolbar-download-menu"]/div[3]')
          crosstab_download_button$clickElement()
        }, error = function(e){
          Sys.sleep(2)
          download_button <- remDr$findElement(using = "xpath", value = '//*[@id="download"]')
          download_button$clickElement()
          crosstab_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="viz-viewer-toolbar-download-menu"]/div[3]')
          crosstab_download_button$clickElement()
        })
      }))
      
      Sys.sleep(2)
      
      csv_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[2]/div[2]/label[2]')
      csv_download_button$clickElement()
      
      Sys.sleep(2)
      
      jurisdiction_values_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[1]/div[2]/div/div/div[4]')
      jurisdiction_values_button$clickElement()
      
      Sys.sleep(2)
      
      final_download_button <- remDr$findElement(using = "xpath", value = '//*[@id="export-crosstab-options-dialog-Dialog-BodyWrapper-Dialog-Body-Id"]/div/div[3]/button')
      final_download_button$clickElement()
      
      Sys.sleep(2)
      
      download_elements <- remDr$findElements(
        using = "xpath",
        value = "//*[contains(text(), 'Download')]"
      )
      
      # Filter for just the "A" tag
      final_download_link <- NULL
      for(elem in download_elements) {
        tag <- elem$getElementAttribute("tagName")[[1]]
        if(tag == "A") {
          final_download_link <- elem
          break
        }
      }
      
      Sys.sleep(2)
      
      print("About to Download")
      
      tryCatch({
        final_download_link$clickElement()
        final_download_link$click()
      }, error = function(e) {
        # message("Error clicking download link: ", e$message)
      })
      
      Sys.sleep(15)
      
      print("Download Complete")
      
      # Define file paths
      input_file <- 
        "data-raw" %>% 
        here("parole-demographic-data") %>% 
        list.files(full.names = TRUE) %>% 
        str_subset("Jurisdiction values.csv$")
      
      output_file <- 
        "data-raw" %>% 
        here("parole-demographic-data") %>% 
        paste0("/jurisdiction_data_",
               hooplah %>% 
                 str_to_lower() %>% 
                 str_replace_all(" ", "_") %>% 
                 str_replace_all("[(]", "") %>% 
                 str_replace_all("[)]", "") %>% 
                 str_replace_all("/", "_"), 
               ".csv")
      
      Sys.sleep(2)
      
      # Read as UTF-16 and write as UTF-8
      con_in <- file(input_file, encoding = "UTF-16")
      content <- readLines(con_in, warn = FALSE)
      close(con_in)
      
      Sys.sleep(1)
      
      con_out <- file(output_file, encoding = "UTF-8")
      writeLines(content, con_out)
      close(con_out)
      
      Sys.sleep(1)
      
      print("UTF8 tranformation complete")
      
      unlink(input_file, recursive = FALSE)
    }
    
    output_file <- 
      "data-raw" %>% 
      here("parole-demographic-data") %>% 
      paste0("/jurisdiction_data_",
             hooplah %>% 
               str_to_lower() %>% 
               str_replace_all(" ", "_") %>% 
               str_replace_all("[(]", "") %>% 
               str_replace_all("[)]", "") %>% 
               str_replace_all("/", "_"), 
             ".csv")
    
    # Now use fread on the new file
    dt <- 
      output_file %>% 
      fread(header = TRUE, stringsAsFactors = FALSE) %>% 
      clean_names() %>% 
      dplyr::select(jurisdiction, measure, variable, starts_with("x")) %>% 
      pivot_longer(cols = starts_with("x"), 
                   names_to = "year", 
                   values_to = "values") %>% 
      mutate(year = 
               year %>% 
               str_replace_all("^x", ""), 
             values = 
               values %>% 
               str_trim(side = "both") %>% 
               na_if("") %>% 
               na_if("##") %>% 
               na_if("~") %>% 
               na_if("/") %>%
               str_replace_all(",", "") %>% 
               str_replace_all("^-", "") %>% 
               str_replace_all("\\\\", "") %>% 
               na_if("") %>%
               as.numeric()) %>% 
      filter(is.na(values) == FALSE)
    
    Sys.sleep(10)
    
    return(dt)
  }) %>% 
  bind_rows()

##Kill Selenium
kill_selenium(port_to_kill = free_port)
Sys.sleep(10)


####Write Data####
##Create Schema
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bjs;")

##Insert into Database
dbWriteTable(con, DBI::Id("bjs", "bjs_parole_characteristics_data"), final_df, overwrite = TRUE)

##Write Compressed CSV
fwrite(final_df, here("data", "bjs_parole_characteristics_data.csv.gz"), compress = "gzip")





