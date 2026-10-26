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
  filter(!(control_type == "Total Correctional Control"))

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

pop_per_capita_df <-
  bjs_data_filtered %>%
  filter(data_variables_clean == "population_per_100000_adult_residents") %>%
  dplyr::select(state, year, supervision_type, per_capita_rate = value)

pop_raw_df <-
  bjs_data_filtered %>%
  filter(data_variables_clean == "population") %>%
  dplyr::select(state, year, supervision_type, population = value) %>% 
  distinct() %>% 
  group_by(state, year, supervision_type) %>% 
  slice(1) %>% 
  ungroup()

bjs_core <-
  bjs_data_filtered %>%
  filter(outcome_category %in% c("Population", "Admissions", "Exits")) %>%
  mutate(is_reported = 
           data_variables_clean %>% 
           str_detect("reported|^population$")) %>%
  dplyr::select(-c(date_to, date_from, table_num, table_name_clean)) %>% 
  distinct() %>% 
  left_join(pop_per_capita_df, 
            by = c("state", "year", "supervision_type")) %>%
  left_join(pop_raw_df,
            by = c("state", "year", "supervision_type")) %>%
  mutate(value_per_100k = 
           case_when(outcome_category == "Population" ~ per_capita_rate,
                     !is.na(population) & population > 0 ~ 100000 * (value / population),
                     TRUE ~ NA_real_))

bjs_dem_df <- 
  "data" %>% 
  here("bjs_parole_characteristics_data.csv.gz") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(supervision_type = "Parole") %>% 
  bind_rows("data" %>% 
              here("bjs_probation_characteristics_data.csv.gz") %>% 
              fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
              mutate(supervision_type = "Probation")) %>% 
  clean_names()

##Combine BJS w/ PPI
ppi_bjs_data <- 
  bjs_core %>% 
  filter(data_variables_title %>% 
           str_detect("Imputed$")) %>% 
  filter(!(state %in% c("U.S. Total", "State Total"))) %>% 
  dplyr::select(state, year, supervision_type, outcome_category, value) %>% 
  rename("state_name" = "state", 
         "control_type" = "supervision_type", 
         "count" = "value") %>% 
  mutate(year = 
           year %>% 
           as.character(),
         data_source = "BJS") %>%   
  bind_rows(ppi_data %>% 
              mutate(outcome_category = "Population",
                     data_source = "PPI")) %>%
  group_by(state_name) %>% 
  tidyr::fill(c(division, region, state_fips, state_fips_numeric, state_abbr, ap_abbr), .direction = "updown") %>% 
  ungroup() %>% 
  group_by(state_name, year) %>% 
  tidyr::fill(c(total_pop), .direction = "updown") %>% 
  ungroup() %>% 
  mutate(rate = 
           ifelse(is.na(rate) == TRUE, 100000 * (count/total_pop), rate)) %>% 
  filter(is.na(count) == FALSE)

##State Geometries
states_sf <- 
  "data" %>% 
  here("states_sf.rds") %>% 
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

##Basemap Helper Function
carto_key <- Sys.getenv("CARTO_API_KEY")

add_basemap <- function(map) {
  if (nzchar(carto_key)) {
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
    addProviderTiles(map, "Esri.WorldGrayCanvas")
  }
}

##Define Quarters
available_years <- 
  ppi_bjs_data %>% 
  pull(year) %>% 
  unique() %>% 
  sort() %>% 
  as.numeric()


####App####
##UI
ui <- navbarPage("National Supervision Data Dashboard",
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
                                   
        /* --- DT FILTER FIX --- */
      .dataTables_filter {
        float: right !important;
        text-align: right !important;
      }
      div.dataTables_wrapper div.row {
        margin-left: 0 !important;
        margin-right: 0 !important;
      }
      div.dataTables_wrapper div.row > div {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }"))),
                 
                 tabPanel("About & Data Dictionary",
                          div(id = "dashboard_about",
                              fluidPage(
                                style = "padding-top: 20px; max-width: 1100px; margin: 0 auto;",
                                
                                h2("About This Dashboard"),
                                p("This dashboard visualizes trends in U.S. correctional supervision —",
                                  "incarceration (jail and prison) and community supervision",
                                  "(probation and parole) — using data from the Prison Policy Initiative (PPI)",
                                  "and the Bureau of Justice Statistics (BJS), covering 1978 through the most",
                                  "recent reporting year."),
                                
                                tags$hr(),
                                
                                h3("How to Use Each Tab"),
                                
                                h4("Main Dashboard"),
                                tags$ul(
                                  tags$li(strong("Map & Barplot: "), "Choose a supervision or control type (e.g. Probation, Parole, ",
                                          "Prison), an outcome category (Population, Admissions, or Exits), and ",
                                          "whether to view totals or per-100,000-residents rates."),
                                  tags$li(strong("Aggregation Type: "), "When viewing a Year Range, values are ",
                                          "aggregated across years using the selected statistic (mean, median, ",
                                          "min, max, or sum for totals)."),
                                  tags$li(strong("Date Selection Mode: "), "Toggle between a single year snapshot ",
                                          "or a range of years to aggregate over."),
                                  tags$li(strong("Table: "), "A sortable, filterable, downloadable (CSV/Excel/copy) ",
                                          "table of the underlying state-year-level records for your current filters.")
                                ),
                                
                                h4("State Dashboard"),
                                tags$ul(
                                  tags$li("Select a single state to see its community supervision, probation, ",
                                          "and parole trends over time, compared against the national mean and ",
                                          "population-weighted national median."),
                                  tags$li("The stacked barplot summarizes that state's average values across the ",
                                          "selected year range, broken out by outcome category and supervision type.")
                                ),
                                
                                h4("National Dashboard"),
                                tags$ul(
                                  tags$li("Aggregate U.S. trends (excluding the federal system) across community ",
                                          "supervision, probation, and parole, either as totals or per-100,000 rates."),
                                  tags$li("The line chart shows year-over-year trends; the bar chart shows the ",
                                          "average over your selected year range.")
                                ),
                                
                                tags$hr(),
                                
                                h3("Data Sources"),
                                tags$ul(
                                  tags$li(strong("Prison Policy Initiative (PPI): "),
                                          "Covers population data for every year, state, and control type ",
                                          "from 1978 to 2024. This builds on, corrects, and extends existing ",
                                          "BJS data — including harmonizing definitions across states and ",
                                          "years, and filling gaps where BJS data was missing, inconsistent, ",
                                          "or known to be inaccurate. Full details on the PPI methodology are ",
                                          "available in their briefing: ",
                                          tags$a(href = "https://www.prisonpolicy.org/blog/2026/06/02/supervision/",
                                                 target = "_blank",
                                                 "Probation and parole trends by state: A look back at the data on 'alternatives' to incarceration"),
                                          "."),
                                  tags$li(strong("Bureau of Justice Statistics (BJS): "),
                                          "Admissions and exits data come directly from the BJS Probation and ",
                                          "Parole annual reports and are only available back to 2012. Because ",
                                          "of this, the State and National Dashboards default to a 2012–2024 ",
                                          "year range, ensuring admissions, exits, and population are all ",
                                          "comparable over the same period. Population data draws on both ",
                                          "BJS and PPI sources, as described above.")
                                ),
                                p(class = "reporting-note",
                                  "Note: Some BJS figures are imputed to account for non-reporting states/years. ",
                                  "Where PPI and BJS overlap, this dashboard combines them to maximize year and ",
                                  "state coverage; always check the Table tab of the Main Dashboard for the underlying values behind ",
                                  "any chart."),
                                
                                tags$hr(),
                                
                                h3("Data Dictionary"),
                                p("Definitions for the key fields used throughout the dashboard:"),
                                DTOutput("data_dictionary_table")
                              )
                          )
                 ), 
                 
                 tabPanel("Main Dashboard",
                          div(id = "dashboard_main", 
                              sidebarLayout(
                                sidebarPanel(
                                  width = 2,
                                  tabsetPanel(
                                    tabPanel(
                                      "Map and Barplot Filters",
                                      br(),
                                      selectInput("rate_or_count", "Count or Per Capita",
                                                  choices = c("Per Capita", "Total Count"),
                                                  selected = "Total Count"),
                                      selectInput("supervision_type", "Supervision Type",
                                                  choices = c("Total Correctional Control", "Community Supervision", 
                                                              "Probation", "Parole", "Total Incarceration",
                                                              "Jail Custody", "Jail Jurisdiction", "Prison"),
                                                  selected = "Community Supervision"),
                                      selectInput("outcome_category", "Outcome Category",
                                                  choices = c("Population", "Admissions", "Exits"),
                                                  selected = "Population"),
                                      selectInput("agg_level", "Aggregation Type",
                                                  choices = c("Mean" = "mean",
                                                              "Median" = "median",
                                                              "Minimum" = "min",
                                                              "Maximum" = "max"),
                                                  selected = "mean"),
                                      div(class = "year-mode-toggle",
                                          radioButtons("year_mode_map", "Date Selection Mode",
                                                       choices = c("Year Range" = "range",
                                                                   "Single Year" = "single"),
                                                       selected = "range",
                                                       inline = FALSE)
                                      ),
                                      uiOutput("year_selector_map")
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
                                              choices = sort(na.omit(unique(ppi_bjs_data$state_name)))),
                                  selectInput("rate_or_count_state", "Count or Per Capita",
                                              choices = c("Per Capita", "Total Count"),
                                              selected = "Total Count"),
                                  sliderTextInput(
                                    inputId = "year_range_state",
                                    label = "Year Range",
                                    choices = available_years,
                                    selected = c(2012, last(available_years)),
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
                                    choices = available_years,
                                    selected = c(2012, last(available_years)),
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
                 )
)

##Server
server <- function(input, output, session){

  output$data_dictionary_table <- renderDT({
    
    dict <- tibble::tribble(
      ~Category,                  ~Term,                        ~Explanation,
      
      # -- Types of Supervision / Incarceration --
      "Supervision & Incarceration Types", "Total Correctional Control",
      "Everyone under any form of state correctional control — incarceration plus community supervision — counted once even if under multiple types at once.",
      "Supervision & Incarceration Types", "Total Incarceration",
      "Combined jail and prison populations.",
      "Supervision & Incarceration Types", "Jail Custody",
      "People physically held in a local jail, regardless of which agency has legal authority over them.",
      "Supervision & Incarceration Types", "Jail Jurisdiction",
      "People under a jail's legal authority, including anyone held elsewhere (e.g. state prison, another facility) on the jail's behalf.",
      "Supervision & Incarceration Types", "Prison",
      "People held in state or federal prison facilities, typically serving longer sentences than jail.",
      "Supervision & Incarceration Types", "Community Supervision",
      "Combined probation and parole populations — people serving their sentence in the community rather than incarcerated.",
      "Supervision & Incarceration Types", "Probation",
      "Court-ordered supervision served in the community, typically instead of incarceration.",
      "Supervision & Incarceration Types", "Parole",
      "Supervised release in the community after serving part of a prison sentence.",
      
      # -- Outcome Categories --
      "Outcome Measures", "Population",
      "A point-in-time count of how many people are under a given type of control (e.g. on probation) as of a specific date.",
      "Outcome Measures", "Admissions",
      "The number of people newly entering a given type of control during the year (e.g. new probation sentences).",
      "Outcome Measures", "Exits",
      "The number of people leaving a given type of control during the year (e.g. completing parole, revocation, etc.).",
      
      # -- Metrics & Aggregation --
      "Metrics & Aggregation", "Total Count",
      "The raw, unadjusted number of people or events — not adjusted for a state's population size.",
      "Metrics & Aggregation", "Per Capita (Rate per 100,000)",
      "Total Count divided by the state's resident population, scaled to a per-100,000-residents basis. Use this to fairly compare states of very different sizes.",
      "Metrics & Aggregation", "Aggregation Type (Mean / Median / Min / Max / Sum)",
      "When viewing a range of years, this is the statistic used to summarize multiple years into a single value per state (e.g. the average rate from 2012–2024).",
      "Metrics & Aggregation", "National Mean",
      "The simple (unweighted) average of a value across all states — every state counts equally regardless of population size.",
      "Metrics & Aggregation", "National Median",
      "The population-weighted median across states — larger states have more influence, giving a truer sense of the 'typical' resident's experience nationally.",
      
      # -- Data & Sources --
      "Data & Sources", "Prison Policy Initiative (PPI)",
      "State-level population data for 1978–2024, compiled and corrected by PPI, which harmonizes definitions across states and fills known BJS data gaps. See the About tab for the source article.",
      "Data & Sources", "BJS Probation & Parole Annual Report",
      "The Bureau of Justice Statistics' annual report on probation and parole, the source for Admissions and Exits data (available from 2012 onward).",
      "Data & Sources", "Imputed vs. Reported",
      "'Reported' figures were submitted directly by a state; 'Imputed' figures are estimated by BJS to fill in for states that didn't report in a given year, so that national and multi-year totals stay complete.",
      
      # -- Reading the Charts --
      "Reading the Charts", "Map shading",
      "Darker green = higher values for your selected filters. Hover over a state to see its exact value. Gray/uncolored states have no data for the current filters.",
      "Reading the Charts", "Main Dashboard barplot",
      "States ranked from lowest to highest for your current filters — read top to bottom for a quick 'most to least' comparison.",
      "Reading the Charts", "State Dashboard line chart",
      "Compares your selected state (blue) against the National Mean (orange) and National Median (green) over time, faceted by outcome category (rows) and supervision type (columns).",
      "Reading the Charts", "State Dashboard stacked bars",
      "Shows your selected state's average value per supervision type over the chosen year range, faceted by outcome category — a quick summary rather than a time trend.",
      "Reading the Charts", "National Dashboard trend lines",
      "Each line is a supervision type's national value over time; note that y-axis scales differ across facets (Population, Admissions, Exits) since their magnitudes vary widely.",
      "Reading the Charts", "Hover text / tooltips",
      "Every chart shows exact underlying values on hover — use this to get precise numbers rather than reading off the axis."
    )
    
    datatable(
      dict,
      class = 'compact stripe hover',
      filter = "top",
      options = list(
        dom = 'ftip',
        paging = FALSE,
        scrollY = "550px",
        scrollX = TRUE,
        scrollCollapse = FALSE,
        columnDefs = list(
          list(width = '18%', targets = 0),
          list(width = '20%', targets = 1),
          list(width = '62%', targets = 2)
        )
      ),
      style = "bootstrap4",
      rownames = FALSE
    )
  })
  
  available_years_map <- reactive({
    req(input$outcome_category)
    ppi_bjs_data %>%
      filter(outcome_category == input$outcome_category) %>%
      pull(year) %>%
      unique() %>%
      sort() %>%
      as.numeric()
  })

  output$year_selector_map <- renderUI({
    yrs <- available_years_map()
    
    if(input$year_mode_map == "single"){
      fluidRow(
        column(7,
               selectInput("single_year_map_val", "Year",
                           choices = yrs,
                           selected = max(yrs))
        )
      )
    } else {
      sliderTextInput(
        inputId = "year_range",
        label = "Year/Quarter Range",
        choices = yrs,
        selected = c(first(yrs), last(yrs)),
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
        )
      )
    }else{
      sliderTextInput(
        inputId = "year_range_table",
        label = "Year/Quarter Range",
        choices = available_years,
        selected = c(first(available_years), last(available_years)),
        grid = FALSE,
        dragRange = TRUE,
        force_edges = TRUE
      )
    }
  })
  
  map_agg_prefix <- reactive({
    if (input$year_mode_map == "single") "" else paste0(str_to_title(input$agg_level), " ")
  })
  
  # Helper: get selected years for map/barplot
  map_selected_years <- reactive({
    if(input$year_mode_map == "single"){
      req(input$single_year_map_val)
      input$single_year_map_val
    }else{
      req(input$year_range)
      year_range <- input$year_range
      if (is.null(year_range)) return(available_years)
      start_idx <- input$year_range[1]
      end_idx   <- input$year_range[2]
      start_idx:end_idx
    }
  })
  
  observeEvent(c(input$rate_or_count), {
    if(input$rate_or_count == "Per Capita"){
      updateSelectInput(session, "agg_level",
                        choices = c("Mean" = "mean",
                                    "Median" = "median",
                                    "Minimum" = "min",
                                    "Maximum" = "max"),
                        selected = "mean")
    }else{
      updateSelectInput(session, "agg_level",
                        choices = c("Mean" = "mean",
                                    "Median" = "median",
                                    "Minimum" = "min",
                                    "Maximum" = "max",
                                    "Sum" = "sum"),
                        selected = "mean")
    }
  })

  filtered_data <- reactive({
    selected_years <- map_selected_years()
    
    ppi_bjs_data %>%
      filter(
        year %in% selected_years,
        control_type == input$supervision_type, 
        outcome_category == input$outcome_category
      )
  })
  
  filtered_data_full <- reactive({
    selected_years <- map_selected_years()
    
    df_table <-
      ppi_bjs_data %>%
      filter(year %in% selected_years) %>% 
      group_by(state_name, year, control_type, outcome_category, data_source) %>%  
      dplyr::summarize(across(c(count, total_pop),
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(rate = 
               100000 * (count/total_pop)) %>% 
      filter(control_type %in% input$supervision_type, 
             outcome_category %in% input$outcome_category) 
    
    if(input$rate_or_count == "Per Capita"){
      df_table %>% 
        mutate(blah = 
                 ifelse(state_name == "Federal", 1, 0)) %>% 
        arrange(blah, desc(rate)) %>% 
        mutate(rate = 
                 rate %>% 
                 round(2),
               count = 
                 count %>% 
                 comma(),
               year = 
                 year %>% 
                 as.character(), 
               across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
        dplyr::select(state_name, year, control_type, outcome_category, rate, count, data_source) %>%  
        rename("state" = "state_name") %>% 
        rename_with(~.x %>% 
                      str_replace_all("^control_type", "Supervision Type") %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title() %>% 
                      str_replace_all("^Rate$", "Rate per 100,000"))
    }else{
      df_table %>% 
        mutate(blah = 
                 case_when(state_name == "Federal" ~ 2, 
                           state_name == "United States" ~ 1, 
                           TRUE ~ 0)) %>% 
        arrange(blah, desc(count)) %>% 
        mutate(rate = 
                 rate %>% 
                 round(2),
               count = 
                 count %>% 
                 comma(),
               year = 
                 year %>% 
                 as.character(), 
               across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .))) %>% 
        dplyr::select(state_name, year, control_type, outcome_category, count, rate, data_source) %>%  
        rename("state" = "state_name") %>% 
        rename_with(~.x %>% 
                      str_replace_all("^control_type", "Supervision Type") %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title() %>% 
                      str_replace_all("^Rate$", "Rate per 100,000"))
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
          group_by(state_name, year, control_type) %>% 
          dplyr::summarize(across(c(count, total_pop), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(rate = 
                   100000 * (count/total_pop)) %>% 
          group_by(state_name) %>%
          dplyr::summarize(value = 
                             rate %>% 
                             mean(na.rm = TRUE)) %>% 
          ungroup()
      }else if(input$agg_level == "median"){
        filtered_data() %>%
          group_by(state_name, year, control_type) %>% 
          dplyr::summarize(across(c(count, total_pop), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(rate = 
                   100000 * (count/total_pop)) %>% 
          group_by(state_name) %>%
          dplyr::summarize(value = 
                             rate %>% 
                             weightedMedian(w = total_pop, 
                                            na.rm = TRUE)) %>% 
          ungroup()
      }else{
        filtered_data() %>%
          group_by(state_name, year, control_type) %>% 
          dplyr::summarize(across(c(count, total_pop), 
                                  ~.x %>% 
                                    sum(na.rm = TRUE))) %>% 
          ungroup() %>% 
          mutate(rate = 
                   100000 * (count/total_pop)) %>% 
          group_by(state_name) %>%
          dplyr::summarize(value = 
                             rate %>% 
                             agg_fun(na.rm = TRUE)) %>%
          ungroup()
      }
    }else{
      filtered_data() %>%
        group_by(state_name, year, control_type) %>% 
        dplyr::summarize(across(c(count, total_pop), 
                                ~.x %>% 
                                  sum(na.rm = TRUE))) %>% 
        ungroup() %>% 
        group_by(state_name) %>% 
        dplyr::summarize(value = 
                           count %>% 
                           agg_fun(na.rm = TRUE)) %>% 
        ungroup()
    }
  })
  
  output$map <- renderLeaflet({
    df_map <- agg_data()
    
    if(input$rate_or_count == "Per Capita"){
      legend_title <- 
        paste0(map_agg_prefix(),
               input$supervision_type, " ",
               "Population per 100,000") %>%
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      map_data <- 
        states_sf %>%
        left_join(df_map, by = "state_name") %>% 
        mutate(value = 
                 value %>% 
                 replace_na(0) %>% 
                 as.numeric())
      
      pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
      
      map_data %>% 
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
    }else{
      legend_title <- 
        paste0(map_agg_prefix(),
               input$supervision_type, " Population") %>%
        str_wrap(width = 30) %>% 
        str_replace_all("\n", "<br>")
      
      map_data <- 
        states_sf %>%
        left_join(df_map, by = "state_name") %>% 
        mutate(value = 
                 value %>% 
                 replace_na(0) %>% 
                 as.numeric())
      
      pal <- colorNumeric(palette = "YlGn", domain = map_data$value, na.color = "transparent")
      
      map_data %>% 
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
    }
  })
  
  output$barplot <- renderPlotly({
    
    if(input$rate_or_count == "Per Capita"){
      x_lab <- paste0(map_agg_prefix(), input$supervision_type, " Population per 100,000")
      
      df <- 
        agg_data() %>%
        filter(!(state_name %in% c("United States", "Federal"))) %>% 
        mutate(across(c(any_of(c("state_name"))), ~replace_na(.x, "Missing/Not Applicable")),
               value = 
                 value %>% 
                 as.numeric()) 
      
      ggplot_barplot_main <- 
        df %>%
        mutate(geo_reorder = reorder(state_name, value)) %>%
        ggplot(aes(
          x = value,
          y = geo_reorder,
          fill = value,
          text = paste0(state_name, ": ", round(value, 2))
        )) +
        geom_col() +  
        scale_fill_distiller(palette = "YlGn", 
                             direction = 1, 
                             name = NULL) +
        labs(
          x = x_lab,
          y = 
            input$geo_level %>%
            str_replace_all("state_name", "State")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            size = 8,
            margin = margin(r = 0)
          ),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10))
        )
    }else{
      x_lab <- paste0(map_agg_prefix(), input$supervision_type, " Population")
      
      df <- 
        agg_data() %>%
        mutate(across(c(any_of(c("state_name"))), ~replace_na(.x, "Missing/Not Applicable")),
               value = 
                 value %>% 
                 as.numeric()) %>% 
        filter(!(state_name == "United States"))
      
      ggplot_barplot_main <- 
        df %>%
        mutate(geo_reorder = reorder(state_name, value)) %>%
        ggplot(aes(
          x = value,
          y = geo_reorder,
          fill = value,
          text = paste0(state_name, ": ", comma(value))
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
            str_replace_all("state_name", "State")
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            size = 8, 
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
    datatable(
      filtered_data_full(),
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
      style = "bootstrap4",
      rownames = FALSE
    )
  })
  
  output$state_viz_vs_national <- renderPlotly({
    req(input$year_range_state)
    if(is.null(input$year_range_state) == TRUE){
      selected_years <- available_years
    }else{
      selected_years <- input$year_range_state[1]:input$year_range_state[2]
    }
    
    state_colors <- c(
      "National Mean"   = "#E69F00",
      "National Median" = "#009E73"
    ) %>% 
      c(setNames("#0072B2", input$state_selection))

    ppi_bjs_data %>%
      filter(year %in% selected_years) %>% 
      filter(!(state_name %in% c("United States", "Federal"))) %>% 
      group_by(state_name, year, control_type, outcome_category) %>% 
      dplyr::summarize(across(c(count, total_pop),
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(rate = 
               100000 * (count/total_pop)) %>% 
      dplyr::select(state_name, year, control_type, outcome_category, rate, count, total_pop) %>% 
      {
        base_data <- .
        base_data %>% 
          bind_rows(base_data %>%  
                      group_by(year, control_type, outcome_category) %>%
                      dplyr::summarize(across(c("rate", "count"), 
                                              ~.x %>% 
                                                mean(na.rm = TRUE))) %>% 
                      ungroup() %>% 
                      mutate(state_name = "National Mean"),
                    base_data %>%
                      group_by(year, control_type, outcome_category) %>%
                      dplyr::summarize(across(c("rate", "count"), 
                                              ~.x %>% 
                                                weightedMedian(w = total_pop, na.rm = TRUE))) %>% 
                      ungroup() %>% 
                      mutate(state_name = "National Median"))
      } %>% 
      filter(state_name %in% c(input$state_selection, "National Mean", "National Median") & 
               year %in% selected_years &
               control_type %in% c("Community Supervision", "Probation", "Parole")) %>%
      mutate(control_type = 
               control_type %>% 
               factor(levels = rev(c("Community Supervision", "Probation", "Parole"))),
             hover_text = paste0(
               "Control Type: ", control_type, "\n",
               "Outcome Category: ", outcome_category, "\n",
               "State: ", state_name, "\n",
               "Value: ", if(input$rate_or_count_state == "Per Capita") 
                 round(rate, 2) 
               else 
                 format(round(count, 0), big.mark = ","), "\n",
               "Year: ", year), 
             year = 
               year %>% 
               as.numeric()) %>%
      {
        ggplot(., aes(x = year,
                      y = if(input$rate_or_count_state == "Per Capita") rate else count,
                      color = state_name,
                      group = state_name,
                      text = hover_text)) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          facet_grid(outcome_category~control_type) +
          scale_color_manual(values = state_colors) +
          scale_x_continuous(
            breaks = scales::breaks_width(4),   
            labels = scales::label_number(big.mark = "")  
          ) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Year", 
               y = if(input$rate_or_count_state == "Per Capita") "Count per 100,000" else "Total Count",
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
    if(is.null(input$year_range_state) == TRUE){
      selected_years <- available_years
    }else{
      selected_years <- input$year_range_state[1]:input$year_range_state[2]
    }
    
    use_per_capita <- input$rate_or_count_state == "Per Capita"
    y_label <- if(use_per_capita) "Mean Count per 100,000 Over Time" else "Mean Count Over Time"
    
    state_colors_alt <- c(
      "Community Supervision" = "#56B4E9",
      "Probation" = "#D55E00",
      "Parole"  = "#CC79A7"
    )
    
    ppi_bjs_data %>%
      filter(year %in% selected_years) %>% 
      filter(!(state_name %in% c("United States", "Federal"))) %>% 
      filter(control_type %in% c("Community Supervision", "Probation", "Parole")) %>% 
      group_by(state_name, year, control_type, outcome_category) %>% 
      dplyr::summarize(across(c(count, total_pop),
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(rate = 
               100000 * (count/total_pop), 
             plot_val = if(use_per_capita) rate else count) %>% 
      dplyr::select(state_name, year, control_type, outcome_category, rate, count, total_pop, plot_val) %>% 
      filter(state_name == input$state_selection) %>% 
      group_by(control_type, outcome_category, state_name) %>% 
      dplyr::summarize(plot_val = 
                         plot_val %>%
                         mean(na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(hover_text = 
               paste0(
                 "Control Type: ", control_type, "\n",
                 "Outcome Category: ", outcome_category, "\n",
                 "State: ", state_name, "\n",
                 "Mean Value: ", format(round(plot_val, if(use_per_capita) 2 else 0), big.mark = ","), "\n")) %>% 
      {
        ggplot(., aes(x = control_type, y = plot_val,
                      fill = control_type, color = control_type,
                      group = control_type, text = hover_text)) +
          geom_col(position = "stack", width = 0.99) +
          scale_color_manual(values = state_colors_alt) +
          scale_fill_manual(values = state_colors_alt) +
          facet_wrap(~outcome_category) + 
          scale_y_continuous(labels = scales::comma) +
          labs(x = NULL, y = y_label, color = "Control Type", fill = "Control Type") +
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
    if(is.null(input$year_range_national) == TRUE){
      selected_years <- available_years
    }else{
      selected_years <- input$year_range_national[1]:input$year_range_national[2]
    }
    
    use_per_capita <- input$rate_or_count_national == "Per Capita"
    y_label <- if(use_per_capita) "Mean Count per 100,000 Over Time" else "Mean Count Over Time"
    
    ppi_bjs_data %>%
      filter(year %in% selected_years) %>% 
      filter(!(state_name == "United States")) %>% 
      mutate(total_pop = ifelse(state_name == "Federal", 0, total_pop)) %>% 
      group_by(year, control_type, outcome_category) %>% 
      dplyr::summarize(across(c(count, total_pop),
                              ~.x %>% 
                                sum(na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(rate = 
               100000 * (count/total_pop)) %>% 
      dplyr::select(year, control_type, outcome_category, rate, count, total_pop)
  })
  
  # National stacked barplot: mean counts over the selected period by supervision type
  output$national_viz_stacked_barplot <- renderPlotly({
    df <- national_base_data()
    
    national_colors <- c(
      "Community Supervision" = "#E69F00",
      "Probation" = "#009E73",
      "Parole" = "#0072B2")
    
    df %>%
      filter(control_type %in% c("Community Supervision", "Probation", "Parole")) %>% 
      group_by(control_type, outcome_category) %>%
      dplyr::summarize(across(c(count, rate), 
                              ~.x %>% 
                                mean(na.rm = TRUE))) %>%
      ungroup() %>% 
      mutate(hover_text = 
               paste0("Supervision: ", control_type, "\n",
                      "Outcome Category: ", outcome_category, "\n",
                      "National Mean Count: ", format(round(count, 0), big.mark = ","), "\n",
                      "National Mean per 10k: ", round(rate, 2))) %>%
      {
        ggplot(., aes(
          x = control_type,
          y = if(input$rate_or_count_national == "Per Capita") rate else count,
          fill = control_type,
          color = control_type,
          text = hover_text
        )) +
          geom_col(position = "stack", width = 0.99) +
          scale_color_manual(values = national_colors) +
          scale_fill_manual(values = national_colors) +
          facet_wrap(~outcome_category) +
          scale_y_continuous(labels = scales::comma) +
          labs(
            x = NULL,
            y = if(input$rate_or_count_national == "Per Capita") "National Mean per 100,000 Over Selected Period" else "National Mean Count Over Selected Period",
            color = "Control Type",
            fill  = "Control Type"
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
  
  output$national_viz_trends <- renderPlotly({
    ylab <- 
      ifelse(input$rate_or_count_national == "Per Capita", 
             "National Count per 100,000",  
             "National Total Count") 
    
    df <- 
      national_base_data() %>%
      filter(control_type %in% c("Community Supervision", "Probation", "Parole")) %>% 
      mutate(hover_text = 
               paste0(
                  "Supervision: ", control_type, "\n",
                  "Outcome Category: ", outcome_category, "\n",
                  "National Total: ", format(count, big.mark = ","), "\n",
                  "Per 10k: ", round(rate, 2), "\n",
                  "Year: ", year)) 
    
    national_colors <- c(
      "Community Supervision" = "#E69F00",
      "Probation" = "#009E73",
      "Parole" = "#0072B2")
    
    df %>%
      mutate(year = 
               year %>% 
               as.numeric()) %>%
      {
        ggplot(., aes(
          x = year,
          y = if(input$rate_or_count_national == "Per Capita") rate else count,
          color = control_type,
          group = control_type,
          text = hover_text
        )) +
          geom_line(linewidth = 1) +
          geom_point(size = 1) +
          facet_wrap(~outcome_category, scales = "free_y", ncol = 3) +
          scale_color_manual(values = national_colors) +
          scale_x_continuous(
            breaks = scales::breaks_width(4),  
            labels = scales::label_number(big.mark = "")  
          ) +
          scale_y_continuous(labels = scales::comma) +
          labs(
            x = "Year",
            y = ylab,
            color = "Control Type"
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
}

####Run App####
shinyApp(ui, server)