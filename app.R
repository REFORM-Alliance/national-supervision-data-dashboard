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


####Home Page Data####
# Everything on the Home tab is computed from ppi_bjs_data, the same object the
# other tabs use, so the numbers on Home always match the dashboards. If the
# data are refreshed the figures update automatically. The *headlines* and
# framing sentences are editorial, though -- re-read them after a data refresh.

supervision_types <- c("Community Supervision", "Probation", "Parole")

# Same colors the National Dashboard uses for these three series
supervision_colors <- c(
  "Community Supervision" = "#E69F00",
  "Probation"             = "#009E73",
  "Parole"                = "#0072B2"
)

# Text-formatting helpers
fmt_count   <- function(x) scales::comma(round(x))
fmt_million <- function(x, digits = 1) paste0(formatC(x / 1e6, format = "f", digits = digits), " million")
fmt_pct     <- function(x, digits = 0) paste0(formatC(abs(x) * 100, format = "f", digits = digits), "%")
dir_word    <- function(x, up = "risen", down = "fallen") ifelse(x >= 0, up, down)
vs_word     <- function(x) ifelse(x >= 0, "above", "below")

# National population by supervision type and year. Mirrors the National
# Dashboard: states + D.C. + the federal system in the count, with the federal
# system left out of the per-capita denominator (its people already live in a state).
landing_trend <-
  ppi_bjs_data %>%
  filter(outcome_category == "Population",
         control_type %in% supervision_types,
         state_name != "United States") %>%
  mutate(year = as.numeric(year),
         total_pop = ifelse(state_name == "Federal", 0, total_pop)) %>%
  group_by(year, control_type) %>%
  dplyr::summarize(count = sum(count, na.rm = TRUE),
                   total_pop = sum(total_pop, na.rm = TRUE),
                   .groups = "drop") %>%
  mutate(rate = 100000 * count / total_pop)

# State community-supervision rates in the latest year (states + D.C.)
landing_state_rates <-
  ppi_bjs_data %>%
  filter(outcome_category == "Population",
         control_type == "Community Supervision",
         !(state_name %in% c("United States", "Federal")),
         year == as.character(max(landing_trend$year))) %>%
  mutate(rate = 100000 * count / total_pop) %>%
  filter(is.finite(rate)) %>%
  dplyr::select(state_name, count, rate) %>%
  arrange(desc(rate))

# BJS admissions, summed the same way as the National Dashboard
landing_admissions <-
  ppi_bjs_data %>%
  filter(data_source == "BJS",
         outcome_category == "Admissions",
         control_type %in% supervision_types) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, control_type) %>%
  dplyr::summarize(count = sum(count, na.rm = TRUE), .groups = "drop")

landing_stats <- local({
  cs <- landing_trend %>% filter(control_type == "Community Supervision") %>% arrange(year)
  pull_count <- function(type, yr) {
    landing_trend %>% filter(control_type == type, year == yr) %>% pull(count)
  }
  adm_change <- function(type, from, to) {
    a <- landing_admissions %>% filter(control_type == type)
    a$count[a$year == to] / a$count[a$year == from] - 1
  }
  
  latest_year <- max(cs$year)
  first_year  <- min(cs$year)
  base_year   <- 2019   # "pre-pandemic" reference year used in the Home text
  peak        <- cs %>% slice_max(count, n = 1, with_ties = FALSE)
  latest      <- cs %>% filter(year == latest_year)
  low_year    <- cs %>% filter(year >= base_year) %>% slice_min(count, n = 1, with_ties = FALSE) %>% pull(year)
  bjs_year    <- max(landing_admissions$year)
  nat_rate    <- latest$rate
  hi          <- landing_state_rates %>% slice_head(n = 1)
  lo          <- landing_state_rates %>% slice_tail(n = 1)
  
  list(
    latest_year = latest_year, first_year = first_year, base_year = base_year,
    peak_year = peak$year, low_year = low_year, bjs_year = bjs_year,
    
    cs_first = cs$count[cs$year == first_year],
    cs_peak = peak$count,
    cs_latest = latest$count,
    cs_latest_rate = nat_rate,
    chg_since_peak = latest$count / peak$count - 1,
    chg_rate_since_peak = nat_rate / peak$rate - 1,
    chg_base_to_low = cs$count[cs$year == low_year] / cs$count[cs$year == base_year] - 1,
    chg_low_to_latest = latest$count / cs$count[cs$year == low_year] - 1,
    chg_base_to_latest = latest$count / cs$count[cs$year == base_year] - 1,
    
    prob_latest = pull_count("Probation", latest_year),
    parole_latest = pull_count("Parole", latest_year),
    prob_share = pull_count("Probation", latest_year) / latest$count,
    prob_chg_low = pull_count("Probation", latest_year) / pull_count("Probation", low_year) - 1,
    parole_chg_low = pull_count("Parole", latest_year) / pull_count("Parole", low_year) - 1,
    prob_chg_base = pull_count("Probation", latest_year) / pull_count("Probation", base_year) - 1,
    parole_chg_base = pull_count("Parole", latest_year) / pull_count("Parole", base_year) - 1,
    
    adm_chg_prob = adm_change("Probation", base_year, bjs_year),
    adm_chg_parole = adm_change("Parole", base_year, bjs_year),
    
    state_hi = hi$state_name, state_hi_rate = hi$rate,
    state_lo = lo$state_name, state_lo_rate = lo$rate,
    state_ratio = hi$rate / lo$rate,
    hi_vs_nat = hi$rate / nat_rate
  )
})
s <- landing_stats   # short alias for the Home-page text below

# Percent change since the base year for the "probation vs. parole" chart.
# 0 = same as the base year; -0.10 = 10% fewer people than in the base year.
landing_change <-
  landing_trend %>%
  filter(control_type %in% c("Probation", "Parole"),
         year >= landing_stats$base_year) %>%
  group_by(control_type) %>%
  mutate(pct_change = count / count[year == landing_stats$base_year] - 1) %>%
  ungroup()

# "down 22%" / "up 5%" -- plain-language change phrase used in headlines and text
chg_phrase <- function(x, digits = 0) paste0(ifelse(x >= 0, "up ", "down "), fmt_pct(x, digits))

# 10 highest and 5 lowest states, with a spacer row between the two groups
landing_state_bars <-
  bind_rows(
    landing_state_rates %>% slice_head(n = 10),
    tibble(state_name = "⋯", count = NA_real_, rate = NA_real_),
    landing_state_rates %>% slice_tail(n = 5)
  )


####Home & About Page Content####
##Styles for the Home and About tabs (added to the page <head> below)
landing_css <- r"---(
/* ===== HOME PAGE ===== */
.lp-page { max-width: 1180px; margin: 0 auto; padding: 24px 20px 48px; }
.lp-hero {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-left: 6px solid #009E73;
  border-radius: 8px; padding: 28px 32px; margin-bottom: 8px;
}
.lp-eyebrow, .lp-trend-num {
  font-size: 12px; letter-spacing: .08em; text-transform: uppercase;
  color: #6B6B6B; font-weight: 700;
}
.lp-hero h1 { font-size: 2rem; font-weight: 700; margin: 6px 0 12px; }
.lp-lede { font-size: 1.1rem; line-height: 1.55; max-width: 880px; margin-bottom: 18px; }
.lp-cta .btn { margin-right: 8px; margin-bottom: 6px; }
.lp-section-title { font-size: 1.4rem; font-weight: 700; margin: 36px 0 4px; }
.lp-section-sub { color: #6B6B6B; margin-bottom: 14px; }
.lp-grid { display: grid; gap: 16px; }
.lp-grid-3 { grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); }
.lp-grid-4 { grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); }
.lp-card {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-radius: 8px; padding: 18px 20px;
}
.lp-card h4 { font-size: 1.05rem; font-weight: 700; margin-bottom: 6px; }
.lp-card p { margin-bottom: 8px; line-height: 1.5; }
.lp-card p:last-child { margin-bottom: 0; }
.lp-tile {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-top: 4px solid #D9CFB4;
  border-radius: 8px; padding: 16px 20px;
}
.lp-tile-value { font-size: 2.3rem; font-weight: 700; line-height: 1.1; }
.lp-tile-label { font-size: .95rem; margin-top: 4px; }
.lp-tile-detail { font-size: .82rem; color: #6B6B6B; margin-top: 6px; }
.lp-trend {
  display: grid; grid-template-columns: minmax(0, 4fr) minmax(0, 7fr); gap: 28px;
  align-items: start; margin: 18px 0; padding: 22px 24px;
  background: #FFFDF7; border: 1px solid #E8DFC8; border-radius: 8px;
}
@media (max-width: 900px) { .lp-trend { grid-template-columns: minmax(0, 1fr); } }
.lp-trend h3 { font-size: 1.3rem; font-weight: 700; margin: 4px 0 10px; }
.lp-trend p { line-height: 1.55; }
.lp-source { font-size: 11px; color: #888; font-style: italic; margin-top: 4px; line-height: 1.4; }
.lp-chart-controls .form-group { margin-bottom: 4px; }
.lp-chart-controls label { font-size: 13px; }
.lp-next .action-button { font-weight: 700; color: #00805C; cursor: pointer; }
.lp-why {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-left: 6px solid #0072B2;
  border-radius: 8px; padding: 18px 24px; margin-top: 16px;
}
.lp-why h4 { font-size: 1.05rem; font-weight: 700; margin-bottom: 6px; }
.lp-why p { line-height: 1.55; margin-bottom: 8px; max-width: 980px; }
.lp-why p:last-child { margin-bottom: 0; }
.lp-why .action-button { font-weight: 700; color: #00805C; cursor: pointer; }

/* ===== ABOUT PAGE ===== */
.about-page { max-width: 1100px; margin: 0 auto; padding: 24px 20px 48px; }
.about-page .nav-pills { margin-bottom: 8px; }
.about-page .nav-pills .nav-link { font-weight: 600; }
.about-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(330px, 1fr)); gap: 16px; }
.about-card {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-radius: 8px; padding: 16px 20px;
}
.about-card h4 { font-size: 1.05rem; font-weight: 700; }
.about-card ol { padding-left: 20px; margin-bottom: 10px; }
.about-card li { margin-bottom: 4px; line-height: 1.45; }
.about-start { font-size: .9rem; margin-bottom: 8px; }
.about-watch { font-size: .88rem; background: #FFF3CC; border-radius: 6px; padding: 8px 10px; margin: 0; }
.about-table { font-size: .92rem; }
.about-table th { border-top: 0; }
.about-page h3 { font-size: 1.25rem; font-weight: 700; margin: 24px 0 8px; }
.about-page li { line-height: 1.5; margin-bottom: 4px; }
.about-callout {
  background: #FFFDF7; border: 1px solid #E8DFC8; border-left: 6px solid #0072B2;
  border-radius: 8px; padding: 14px 20px; margin: 12px 0 16px; max-width: 980px;
}
.about-callout p { line-height: 1.55; margin-bottom: 8px; }
.about-callout p:last-child { margin-bottom: 0; }
.about-page .about-table td:first-child { font-weight: 600; }
)---"

##Helpers for building the Home and About pages
# Join text pieces into ONE HTML string. If pieces are passed to p() separately,
# the browser inserts a space between them (e.g. "in 2024 ."), so sentences that
# mix text, numbers, and bold/links are assembled with these helpers instead.
lp_html <- function(...) HTML(paste0(...))
bold    <- function(...) paste0("<strong>", ..., "</strong>")
ital    <- function(...) paste0("<em>", ..., "</em>")
link_to <- function(url, text) paste0("<a href=\"", url, "\" target=\"_blank\">", text, "</a>")

lp_tile <- function(value, label, detail) {
  div(class = "lp-tile",
      div(class = "lp-tile-value", value),
      div(class = "lp-tile-label", label),
      div(class = "lp-tile-detail", detail))
}

use_card <- function(title, start_at, steps, watch = NULL) {
  div(class = "about-card",
      h4(title),
      p(class = "about-start", strong("Start at: "), start_at),
      tags$ol(lapply(steps, tags$li)),
      if (!is.null(watch)) p(class = "about-watch", strong("Watch out for: "), watch))
}

simple_table <- function(df) {
  tags$table(
    class = "table table-sm about-table",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(df[i, ], function(cell) tags$td(cell)))
    }))
  )
}

dict_datatable <- function(df, widths, filter = "top") {
  datatable(
    df,
    class = "compact stripe hover",
    filter = filter,
    rownames = FALSE,
    style = "bootstrap4",
    options = list(
      dom = if (filter == "none") "t" else "ft",
      paging = FALSE,
      ordering = FALSE,
      columnDefs = lapply(seq_along(widths) - 1, function(i) {
        list(width = widths[i + 1], targets = i)
      })
    )
  )
}

##Glossary (Key Terms & Phrases)
glossary_df <- tibble::tribble(
  ~Category, ~Term, ~Explanation,
  
  # -- Types of supervision and incarceration --
  "Supervision & Incarceration Types", "Community Supervision",
  "Probation plus parole: people serving a sentence or a period of release in the community under a supervision agency's oversight. People on both at the same time are counted in each, so the combined total can count some individuals twice.",
  "Supervision & Incarceration Types", "Probation",
  "A court-ordered period of supervision served in the community, typically instead of incarceration. Federal probation is reported on the 'Federal' line, not in any state.",
  "Supervision & Incarceration Types", "Parole",
  "Supervised release in the community after serving part of a prison sentence. Federal parole is reported on the 'Federal' line and is not divided among states.",
  "Supervision & Incarceration Types", "Prison",
  "People under the jurisdiction of a state prison system, typically serving longer sentences than people in jail. Federal prisons and detention centers are reported on the 'Federal' line.",
  "Supervision & Incarceration Types", "Jail Custody",
  "People physically held in a local jail, regardless of which agency has legal authority over them. In states where jails and prisons form one unified system, this also includes people awaiting trial or serving short sentences.",
  "Supervision & Incarceration Types", "Jail Jurisdiction",
  "People under a jail's legal authority, including anyone held elsewhere on its behalf (for example, in a state prison or another county's jail).",
  "Supervision & Incarceration Types", "Total Incarceration",
  "Jail jurisdiction plus prison jurisdiction. Jurisdiction counts (not custody) are used so that people held in a local jail on behalf of a state prison system or federal agency are not counted twice.",
  "Supervision & Incarceration Types", "Total Correctional Control",
  "PPI's combined measure: total incarceration plus community supervision. It does not adjust for people under more than one type of control. It is not currently charted in this dashboard, even though it appears in the Main Dashboard menu.",
  "Supervision & Incarceration Types", "Federal",
  "The federal system. Federal probation, parole, prisons, and detention are reported on their own 'Federal' line rather than assigned to states. Federal counts are included in National Dashboard totals, but are left out of state means and medians and have no per-capita rate of their own.",
  
  # -- Outcome measures --
  "Outcome Measures", "Population",
  "A snapshot count of how many people are under a given type of control at a point in time. Timing varies: community supervision counts are generally as of the end of the year (January 1 of the following year), prison counts are year-end, and jail counts are generally mid-year.",
  "Outcome Measures", "Admissions",
  paste0("People starting a new term of a given type of supervision during the year (BJS calls these 'entries'). Available only for Community Supervision, Probation, and Parole, 2012–", landing_stats$bjs_year, "."),
  "Outcome Measures", "Exits",
  "People leaving a given type of supervision during the year — by completing their term, being revoked to incarceration, absconding, dying, or for other reasons (BJS calls these 'exits'). Same coverage as Admissions.",
  "Outcome Measures", "Stock vs. flow",
  "Population is a stock (a snapshot); Admissions and Exits are flows (movement during the year). A system with short supervision terms can have large flows but a modest population. When admissions exceed exits in a year, the population grows.",
  
  # -- Supervision vocabulary --
  "Supervision Vocabulary", "Revocation",
  "When a court or paroling authority ends someone's community supervision because of a violation or a new offense, often sending them to jail or prison.",
  "Supervision Vocabulary", "Technical violation",
  "Breaking a rule of supervision — such as missing an appointment or failing a drug test — that is not itself a new crime. Technical violations can lead to revocation.",
  "Supervision Vocabulary", "Absconder",
  "A person under supervision who stops reporting and whose whereabouts are unknown to the supervising agency.",
  "Supervision Vocabulary", "Dual supervision",
  "Being on probation and parole at the same time. These people appear in both counts, and therefore twice in the Community Supervision total.",
  
  # -- Metrics and aggregation --
  "Metrics & Aggregation", "Total Count",
  "The raw, unadjusted number of people or events — not adjusted for a state's population size. Larger states naturally have larger counts.",
  "Metrics & Aggregation", "Per Capita (Rate per 100,000)",
  "Total Count divided by the state's resident population (all ages), multiplied by 100,000. Use this to fairly compare states of very different sizes. Because the denominator includes children, these rates are lower than the per-adult rates BJS publishes.",
  "Metrics & Aggregation", "Aggregation Type (Mean / Median / Min / Max / Sum)",
  "When viewing a range of years in the Main Dashboard, this is the statistic used to summarize multiple years into a single value per state (for example, the average rate from 2012–2024). Sum is offered only for Total Count.",
  "Metrics & Aggregation", "National Mean",
  "The simple (unweighted) average across the states and D.C. — every state counts equally regardless of population size. The federal system is not included.",
  "Metrics & Aggregation", "National Median",
  "A population-weighted median across the states and D.C.: the value that splits the nation's residents in half. Larger states have more influence, giving a better sense of the typical resident's experience.",
  "Metrics & Aggregation", "Percent change",
  paste0("Used on the Home page to compare probation and parole since ", landing_stats$base_year, ". Each line shows how much a population has grown or shrunk relative to its own ", landing_stats$base_year, " level: 0% means no change, and −10% means 10% fewer people than in ", landing_stats$base_year, ". This lets groups of very different sizes be compared on one scale."),
  
  # -- Data and sources --
  "Data & Sources", "Prison Policy Initiative (PPI)",
  paste0("Source of every population count, 1978–", landing_stats$latest_year, ". PPI builds on BJS data, harmonizing definitions across states and years and filling gaps where BJS data were missing, inconsistent, or known to be inaccurate."),
  "Data & Sources", "Why PPI and not BJS for population counts?",
  "BJS is the official federal source, but its published state probation and parole counts have known problems: private probation is largely left out, definitions shift from year to year, and imputed or uncorrected values can create false jumps. PPI starts from BJS's data and corrects it using state and local sources, so this dashboard uses PPI's version for every population count. See Sources & Caveats for details.",
  "Data & Sources", "BJS Probation & Parole Surveys",
  paste0("The Bureau of Justice Statistics' annual surveys of probation and parole agencies. The source of Admissions and Exits data, 2012–", landing_stats$bjs_year, "."),
  "Data & Sources", "Imputed vs. Reported",
  "'Reported' figures were submitted directly by a state; 'Imputed' figures are estimated by BJS to fill in for states that didn't report in a given year, so that national totals stay complete. This dashboard uses the imputed series for Admissions and Exits.",
  
  # -- Reading the charts --
  "Reading the Charts", "Map shading",
  "Darker green = higher values for your selected filters. Hover over a state to see its exact value. States with no data for the current filters are drawn as 0 (the lightest shade) — check the Table to tell a true zero from missing data.",
  "Reading the Charts", "Main Dashboard barplot",
  "States ranked by value for your current filters, highest at the top.",
  "Reading the Charts", "State Dashboard line chart",
  "Compares your selected state (blue) against the National Mean (orange) and National Median (green) over time, with outcome categories in rows and supervision types in columns.",
  "Reading the Charts", "State Dashboard stacked bars",
  "Your selected state's average value per supervision type over the chosen year range, split by outcome category — a quick summary rather than a time trend.",
  "Reading the Charts", "National Dashboard trend lines",
  "Each line is a supervision type's national value over time. Y-axis scales differ across panels (Population, Admissions, Exits) because their magnitudes differ widely.",
  "Reading the Charts", "Home page charts",
  paste0("Simplified views of the same data: a trend line with a count/rate toggle, the percent change in probation and parole since ", landing_stats$base_year, ", and the highest and lowest state rates."),
  "Reading the Charts", "Hover text / tooltips",
  "Every chart shows exact underlying values on hover — use this to get precise numbers rather than reading them off the axis."
)

##Data dictionary (fields you will see in the Table and charts)
fields_df <- tibble::tribble(
  ~Field, ~`Where you'll see it`, ~Type, ~`What it contains`, ~Notes,
  
  "State", "Table, map, bar plots, State Dashboard", "Text",
  "The jurisdiction the figure describes.",
  "The 50 states and the District of Columbia. The Table can also show 'Federal' (the federal system) and 'United States' (PPI's national row).",
  
  "Year", "Table, year sliders", "Integer",
  "The calendar year of the figure.",
  paste0("Population: 1978–", landing_stats$latest_year, " (PPI). Admissions and Exits: 2012–", landing_stats$bjs_year, " (BJS). Counting dates differ by supervision type; see 'Population' under Key Terms."),
  
  "Supervision Type", "Table, filters", "Category",
  "The kind of supervision or incarceration being counted.",
  "Community Supervision, Probation, Parole, Prison, Jail Custody, Jail Jurisdiction, or Total Incarceration.",
  
  "Outcome Category", "Table, filters", "Category",
  "What is being counted: Population, Admissions, or Exits.",
  "Admissions and Exits exist only for Community Supervision, Probation, and Parole.",
  
  "Count", "Table; charts set to Total Count", "Number",
  "The number of people (Population) or of movements into or out of supervision (Admissions, Exits).",
  "Admissions and Exits use BJS's imputed estimates. Values shown on the map and bar plot are summaries (mean, median, etc.) of these yearly counts.",
  
  "Rate per 100,000", "Table; charts set to Per Capita", "Number",
  "Count divided by the state's resident population (all ages), multiplied by 100,000.",
  "Calculated by the dashboard from the count and PPI's state population totals (U.S. Census, via CDC SEER). Blank for the federal system, which has no population of its own.",
  
  "Data Source", "Table", "Text",
  "Where the figure came from.",
  "PPI (population counts) or BJS (admissions and exits)."
)

##Coverage: what is available, by supervision type
coverage_df <- tibble::tribble(
  ~`Supervision Type`, ~Population, ~`Admissions & Exits`, ~`Coverage notes`,
  
  "Community Supervision, Probation, Parole",
  paste0("1978–", landing_stats$latest_year),
  paste0("2012–", landing_stats$bjs_year),
  "Population is complete for all 50 states, D.C., and the federal system in every year. Admissions and Exits are missing for a few state-years (for example, Georgia and Michigan in 2016), which understates national totals in those years.",
  
  "Prison",
  paste0("1978–", landing_stats$latest_year),
  "Not available",
  paste0("Complete through ", landing_stats$latest_year - 1, ". In ", landing_stats$latest_year, ", six states (Alaska, Connecticut, Delaware, Hawaii, Rhode Island, and Vermont) have no prison figure, so ", landing_stats$latest_year, " national prison totals are understated."),
  
  "Jail Custody, Jail Jurisdiction",
  paste0("1978–", landing_stats$latest_year),
  "Not available",
  "Complete through 2019. From 2020 on, only 20 to 26 of the 51 jurisdictions (50 states and D.C.) have data in a given year, so national or multi-state totals are not comparable to earlier years.",
  
  "Total Incarceration",
  paste0("1978–", landing_stats$latest_year),
  "Not available",
  "Complete through 2019. From 2020 on, only 26 of 51 jurisdictions have data, because it requires both jail and prison figures."
)

##Table of controls for the About page
controls_df <- tibble::tribble(
  ~Control, ~`Where`, ~`What it does`,
  "Supervision Type", "Main Dashboard", "Which kind of supervision or incarceration to show: Community Supervision, Probation, Parole, Prison, Jail Custody, Jail Jurisdiction, or Total Incarceration.",
  "Outcome Category", "Main Dashboard", "Population (a snapshot), Admissions (people entering), or Exits (people leaving). Admissions and Exits are available only for community supervision, probation, and parole.",
  "Count or Per Capita", "All dashboards", "Total Count shows raw numbers, so larger states look bigger. Per Capita shows people per 100,000 residents so states of different sizes can be compared fairly.",
  "Date Selection Mode", "Main Dashboard", "Single Year gives a snapshot. Year Range combines several years into one value per state using the Aggregation Type.",
  "Aggregation Type", "Main Dashboard", "How a year range is summarized for each state: Mean, Median, Minimum, Maximum — or Sum when viewing Total Count.",
  "Year Range", "State and National Dashboards", "Drag the ends of the slider. Population goes back to 1978; Admissions and Exits start in 2012, which is why both dashboards open on 2012 to the latest year.",
  "State", "State Dashboard", "The state to compare against the National Mean and National Median.",
  "Table", "Bottom of the Main Dashboard", "Every state-year row behind the current map and bar plot. Search, filter, and download as Copy, CSV, or Excel."
)

##Why PPI and not BJS: problems with BJS's published state counts, as documented in
## PPI's report (Kang-Brown, June 2, 2026), and what PPI did about each. The figures
## quoted here are PPI's; the dashboard did not re-derive them from BJS's files.
ppi_report_url <- "https://www.prisonpolicy.org/blog/2026/06/02/supervision/"

ppi_vs_bjs_df <- tibble::tribble(
  ~`Problem in BJS's published state counts`, ~`Example, from PPI's report`, ~`What PPI did`,
  
  "Private probation is largely missing",
  "BJS collects private probation only where a state agency already does. PPI estimates that about one in three people on probation in Tennessee is supervised by a private company.",
  "Added outside sources where they exist, such as state audit and fee data and Human Rights Watch's private-probation data.",
  
  "Coverage changes look like growth",
  "BJS's 2022 survey expansion added local and misdemeanor probation agencies, so caseloads in Oklahoma, Tennessee, and Virginia appeared to jump.",
  "Rebuilt those states' series from the best available state and local sources. In the corrected data, their probation counts show no sudden jump around 2022 and have trended down in recent years.",
  
  "Definitions change from year to year",
  "Illinois alternately counted and left out administrative-only probation. New Jersey's count fell 77% between BJS's 2022 and 2023 reports.",
  "Reconciled definitions, for example by using archived active-probation counts for New Jersey.",
  
  "Imputed values create false spikes",
  "BJS fills in missing values, which can produce one-year jumps. Delaware's reported 85% rise in 2023 is one example of an apparent spike that other state sources did not show.",
  "Replaced the questionable values with state-source counts, or dropped them and interpolated between neighboring years.",
  
  "State corrections never reach BJS",
  "States such as California, Illinois, Maryland, New York, and Pennsylvania revise their own data, but BJS is not set up to collect those revisions.",
  "Used the states' revised figures. California's peak probation count, for example, is lower than BJS reports.",
  
  "Counting units differ",
  "New York's 1990s figures counted cases rather than people and were substantially inflated.",
  "Used New York State data and interpolated 1987 to 1997.",
  
  "The net effect can be large",
  "Uncorrected BJS data would suggest that Illinois cut its community supervision rate by 42% from 2009 to 2024.",
  "PPI's corrected data show a drop of about 3%, from 1,019 to 989 per 100,000 residents."
)

##Home tab
home_tab <- tabPanel(
  "Home", value = "home",
  div(id = "dashboard_home", class = "lp-page",
      
      # ---- Hero ----
      div(class = "lp-hero",
          div(class = "lp-eyebrow", "U.S. probation, parole, and incarceration data"),
          h1("Who is under correctional supervision in America?"),
          p(class = "lp-lede",
            lp_html("This dashboard brings together state-level data on people on probation, on parole, and in jail ",
                    "or prison — every state, the District of Columbia, and the federal system — from ",
                    s$first_year, " through ", s$latest_year, ". Use it to see how supervision has changed over time, ",
                    "how states compare, and where the data are strongest and weakest.")),
          div(class = "lp-cta",
              actionButton("go_main", "Explore the map", class = "btn-primary"),
              actionButton("go_state", "Look up a state", class = "btn-outline-primary"),
              actionButton("go_national", "See national trends", class = "btn-outline-primary"),
              actionButton("go_about", "Definitions & data dictionary", class = "btn-outline-primary"))
      ),
      
      # ---- About the data ----
      h2(class = "lp-section-title", "About the data"),
      div(class = "lp-grid lp-grid-3",
          div(class = "lp-card",
              h4("What it covers"),
              p(lp_html("People on probation and parole (together called ", bold("community supervision"),
                        ") and people held in jail or prison. Each figure is either a count of people on a given ",
                        "date (", ital("population"), ") or a count of people entering or leaving supervision during ",
                        "a year (", ital("admissions"), " and ", ital("exits"), "), shown as a total or per 100,000 residents."))),
          div(class = "lp-card",
              h4("Where it comes from"),
              p(lp_html("Population counts for ", s$first_year, "–", s$latest_year, " come from the ",
                        bold("Prison Policy Initiative (PPI)"), ", which builds on Bureau of Justice Statistics (BJS) ",
                        "data by harmonizing definitions across states and years and filling gaps. Admissions and ",
                        "exits (2012–", s$bjs_year, ") come from the ", bold("BJS"),
                        " annual Probation and Parole surveys."))),
          div(class = "lp-card",
              h4("What to keep in mind"),
              p("Probation and parole figures are complete for every state and year, so the trends below ",
                "focus on them. Incarceration data have gaps — especially jail data since 2020 — so ",
                "check the coverage table on the About tab before drawing conclusions. Rates are per 100,000 ",
                "residents of all ages, not just adults."))
      ),
      
      # Why PPI and not BJS: short version (the full explanation is on About > Sources & Caveats)
      div(class = "lp-why",
          h4("Why population counts come from PPI instead of BJS"),
          p(lp_html("The Bureau of Justice Statistics is the official federal source, but its published state ",
                    "probation and parole counts have known problems: private probation is largely left out, ",
                    "definitions change from year to year, and imputed or uncorrected values can create false ",
                    "jumps in a state's trend. The Prison Policy Initiative starts from BJS's data and corrects it ",
                    "using state and local sources, with corrections for 21 states. This dashboard therefore uses ",
                    "PPI's version for every population count, and BJS only for admissions and exits, which PPI's ",
                    "state dataset does not include. See PPI's report, ",
                    link_to(ppi_report_url, "Probation and parole trends by state"),
                    " (Jacob Kang-Brown, June 2, 2026).")),
          p(actionLink("why_ppi_more", "How PPI's numbers differ from BJS's, and what they can't fix →"))
      ),
      
      # ---- At a glance ----
      h2(class = "lp-section-title", paste0("At a glance: ", s$latest_year)),
      p(class = "lp-section-sub", "Community supervision means probation plus parole."),
      div(class = "lp-grid lp-grid-4",
          lp_tile(fmt_million(s$cs_latest),
                  paste0("people on probation or parole in ", s$latest_year),
                  paste0(fmt_count(s$cs_latest_rate), " per 100,000 residents")),
          lp_tile(fmt_pct(s$chg_since_peak),
                  paste0(vs_word(s$chg_since_peak), " the ", s$peak_year, " peak of ", fmt_million(s$cs_peak)),
                  paste0(fmt_pct(s$chg_rate_since_peak), " ", vs_word(s$chg_rate_since_peak), " the peak per capita")),
          lp_tile(fmt_pct(s$prob_share),
                  "of people on community supervision are on probation",
                  paste0(fmt_million(s$prob_latest), " on probation · ", fmt_million(s$parole_latest), " on parole")),
          lp_tile(paste0(formatC(s$state_ratio, format = "f", digits = 1), "×"),
                  "gap between the highest and lowest state rates",
                  paste0(s$state_hi, " ", fmt_count(s$state_hi_rate), " vs. ", s$state_lo, " ",
                         fmt_count(s$state_lo_rate), " per 100,000"))
      ),
      
      # ---- Trends ----
      h2(class = "lp-section-title", "Three trends worth knowing"),
      p(class = "lp-section-sub", "The charts are interactive — hover for exact values."),
      
      # Trend 1: growth then decline
      div(class = "lp-trend",
          div(
            div(class = "lp-trend-num", "Trend 1"),
            h3("Supervision grew nearly fourfold, then reversed"),
            p(lp_html("The number of people on probation or parole climbed from ", bold(fmt_million(s$cs_first)),
                      " in ", s$first_year, " to a peak of ", bold(fmt_million(s$cs_peak)), " in ", s$peak_year,
                      ". By ", s$latest_year, " it stood at ", bold(fmt_million(s$cs_latest)), ": ",
                      bold(fmt_pct(s$chg_since_peak)), " ", vs_word(s$chg_since_peak), " the peak, or ",
                      bold(fmt_pct(s$chg_rate_since_peak)), " ", vs_word(s$chg_rate_since_peak),
                      " it once adjusted for population growth.")),
            p(lp_html("The decline was gradual through the 2010s and then sharper: between ", s$base_year, " and ",
                      s$low_year, " the total ", dir_word(s$chg_base_to_low, "rose", "fell"), " ",
                      fmt_pct(s$chg_base_to_low), ". Since ", s$low_year, " it has ",
                      dir_word(s$chg_low_to_latest), " by ", fmt_pct(s$chg_low_to_latest, 1), "."))
          ),
          div(
            div(class = "lp-chart-controls",
                radioButtons("landing_metric", label = NULL,
                             choices = c("Total count" = "count", "Per 100,000 residents" = "rate"),
                             selected = "count", inline = TRUE)),
            div(role = "img",
                `aria-label` = paste0("Line chart of the U.S. population on community supervision, probation, and parole, ",
                                      s$first_year, " to ", s$latest_year, ". Community supervision peaked at ",
                                      fmt_million(s$cs_peak), " in ", s$peak_year, " and was ",
                                      fmt_million(s$cs_latest), " in ", s$latest_year, "."),
                plotlyOutput("landing_trend_plot", height = "380px")),
            p(class = "lp-source",
              "Source: Prison Policy Initiative. Includes the federal system. People on both probation and ",
              "parole are counted in each.")
          )
      ),
      
      # Trend 2: probation vs parole
      div(class = "lp-trend",
          div(
            div(class = "lp-trend-num", "Trend 2"),
            h3(paste0("Since ", s$base_year, ", parole is ", chg_phrase(s$parole_chg_base),
                      " and probation is ", chg_phrase(s$prob_chg_base))),
            p(lp_html("Both populations are smaller than before the pandemic, but not by the same amount. In ",
                      s$latest_year, ", about ", bold(fmt_million(s$prob_latest, 2)), " people were on probation, ",
                      bold(fmt_pct(s$prob_chg_base)), " ", vs_word(s$prob_chg_base), " the ", s$base_year,
                      " level, and about ", bold(fmt_count(round(s$parole_latest, -3))), " were on parole, ",
                      bold(fmt_pct(s$parole_chg_base)), " ", vs_word(s$parole_chg_base), ".")),
            p(lp_html("The two have also moved differently lately. From ", s$low_year, " to ", s$latest_year,
                      ", the probation population ", dir_word(s$prob_chg_low, "grew", "shrank"), " ",
                      bold(fmt_pct(s$prob_chg_low, 1)), " while parole ", dir_word(s$parole_chg_low, "grew", "shrank"), " ",
                      bold(fmt_pct(s$parole_chg_low, 1)), ". Admissions show the same split: between ", s$base_year,
                      " and ", s$bjs_year, ", annual admissions to parole ", dir_word(s$adm_chg_parole, "rose", "fell"),
                      " ", bold(fmt_pct(s$adm_chg_parole)), ", compared with ", bold(fmt_pct(s$adm_chg_prob)),
                      " for probation."))
          ),
          div(
            div(role = "img",
                `aria-label` = paste0("Line chart of the percent change in the probation and parole populations since ",
                                      s$base_year, ". By ", s$latest_year, ", probation is ", fmt_pct(s$prob_chg_base), " ",
                                      vs_word(s$prob_chg_base), " its ", s$base_year, " level and parole is ",
                                      fmt_pct(s$parole_chg_base), " ", vs_word(s$parole_chg_base), "."),
                plotlyOutput("landing_change_plot", height = "340px")),
            p(class = "lp-source",
              lp_html("Source: Prison Policy Initiative (population); BJS (admissions, which are estimates and ",
                      "are missing for some states in some years). Each line shows the percent change in that group's ",
                      "population compared with its own ", s$base_year, " level: 0% means no change, and −10% ",
                      "means 10% fewer people than in ", s$base_year, "."))
          )
      ),
      
      # Trend 3: state variation
      div(class = "lp-trend",
          div(
            div(class = "lp-trend-num", "Trend 3"),
            h3("Where you live matters: state rates differ more than tenfold"),
            p(lp_html("In ", s$latest_year, ", ", bold(s$state_hi), " had ", bold(fmt_count(s$state_hi_rate)),
                      " people on community supervision per 100,000 residents — ",
                      bold(paste0(formatC(s$hi_vs_nat, format = "f", digits = 1), " times")),
                      " the national rate of ", fmt_count(s$cs_latest_rate), ". ", s$state_lo, " had ",
                      bold(fmt_count(s$state_lo_rate)), ". Across all states and D.C., the highest rate is ",
                      bold(paste0(formatC(s$state_ratio, format = "f", digits = 1), " times")), " the lowest.")),
            p("Use the Main Dashboard map to see every state, or the State Dashboard to compare one state ",
              "with the national mean and median.")
          ),
          div(
            div(role = "img",
                `aria-label` = paste0("Bar chart of the 10 highest and 5 lowest state rates of community supervision per 100,000 residents in ",
                                      s$latest_year, ". Highest: ", s$state_hi, " at ", fmt_count(s$state_hi_rate),
                                      ". Lowest: ", s$state_lo, " at ", fmt_count(s$state_lo_rate), "."),
                plotlyOutput("landing_states_plot", height = "440px")),
            p(class = "lp-source",
              lp_html("Source: Prison Policy Initiative, ", s$latest_year, ". States and D.C.; the federal system is not ",
                      "included. Shows the 10 highest and 5 lowest rates; darker bars are higher rates."))
          )
      ),
      
      # ---- Where next ----
      h2(class = "lp-section-title", "Where to go next"),
      div(class = "lp-grid lp-grid-4 lp-next",
          div(class = "lp-card", h4("Main Dashboard"),
              p("Map and rank every state for any supervision type, outcome, and year, and download the data."),
              actionLink("next_main", "Open the map →")),
          div(class = "lp-card", h4("State Dashboard"),
              p("See one state's trends alongside the national mean and median."),
              actionLink("next_state", "Pick a state →")),
          div(class = "lp-card", h4("National Dashboard"),
              p("Population, admissions, and exits for the nation over time."),
              actionLink("next_national", "See the trends →")),
          div(class = "lp-card", h4("About"),
              p("Ways to use the tool, key terms, a data dictionary, and data caveats."),
              actionLink("next_about", "Read the guide →"))
      )
  )
)

##About tab
about_tab <- tabPanel(
  "About", value = "about",
  div(id = "dashboard_about", class = "about-page",
      
      h2("About This Dashboard"),
      p(class = "lp-lede",
        "How to get the most out of the dashboard, the vocabulary you will run into, and exactly what ",
        "is (and is not) in the data."),
      
      tabsetPanel(
        id = "about_tabs", type = "pills",
        
        # ---- Ways to use it ----
        tabPanel(
          "Ways to Use It",
          h3("Common questions and where to answer them"),
          div(class = "about-grid",
              use_card("Get the national picture",
                       "Home, then the National Dashboard.",
                       c("Read the three trend sections on the Home tab for the headline story.",
                         "Open the National Dashboard and choose Per Capita to remove the effect of population growth, or Total Count to see the number of people.",
                         "Drag the Year Range slider back to 1978 to see the full history of population. Admissions and exits begin in 2012."),
                       "The national admissions and exits lines dip in 2016 largely because Georgia and Michigan are missing that year."),
              use_card("Compare states on one measure",
                       "Main Dashboard.",
                       c("Pick a Supervision Type (for example, Probation) and an Outcome Category.",
                         "Choose Per Capita for a fair comparison between large and small states.",
                         "Switch Date Selection Mode to Single Year for a snapshot, or Year Range to summarize several years.",
                         "Hover over the map or the bars for exact values. The bar plot ranks states from highest to lowest."),
                       "Total Count mostly ranks states by size. States with no data appear as 0 on the map; check the Table to confirm."),
              use_card("See one state in context",
                       "State Dashboard.",
                       c("Choose a state, then Count or Per Capita.",
                         "The bars at the top summarize the state's average over the selected years.",
                         "The line chart below compares the state with the National Mean and National Median over time."),
                       "The National Mean treats every state equally; the National Median is weighted by population, so large states count more. Use both to see how a state compares."),
              use_card("Check whether supervision is growing or shrinking",
                       "Main Dashboard (Outcome Category: Admissions, then Exits), or the State and National Dashboards.",
                       c("Select Community Supervision, Probation, or Parole.",
                         "Compare Admissions (people coming in) with Exits (people leaving). When admissions exceed exits, the population grows.",
                         "The State and National Dashboards show Population, Admissions, and Exits side by side."),
                       paste0("Admissions and exits are BJS estimates available only for 2012–", s$bjs_year, ", and some states are missing in some years.")),
              use_card("Compare supervision with incarceration",
                       "Main Dashboard.",
                       c("Set Supervision Type to Prison, Jail Custody, Jail Jurisdiction, or Total Incarceration.",
                         "Switch to Probation or Parole with the same year and Per Capita setting to compare."),
                       "Jail data cover only 20 to 26 of 51 jurisdictions from 2020 on, and six states have no 2024 prison figure. Prefer 2019 or earlier for nationwide comparisons."),
              use_card("Download the data for your own analysis",
                       "Main Dashboard, Table section at the bottom.",
                       c("Set the filters for the slice you need.",
                         "Sort or filter the table by state, year, or source.",
                         "Use the Copy, CSV, or Excel buttons to export."),
                       "The table reflects your current filters. Change Supervision Type or Outcome Category to export a different slice. The Data Source column shows whether each figure is from PPI or BJS.")
          ),
          
          h3("Controls at a glance"),
          simple_table(controls_df)
        ),
        
        # ---- Key terms ----
        tabPanel(
          "Key Terms & Phrases",
          h3("Key terms and phrases"),
          p("Definitions for the words and phrases used throughout the dashboard. Use the search box or the ",
            "column filters to find a term."),
          DTOutput("glossary_table")
        ),
        
        # ---- Data dictionary ----
        tabPanel(
          "Data Dictionary",
          h3("Fields"),
          p("The fields you will see in the Table and behind the charts."),
          DTOutput("data_dictionary_table"),
          h3("What data are available?"),
          p("Coverage differs by supervision type. Check it before comparing across types or years."),
          DTOutput("coverage_table")
        ),
        
        # ---- Sources and caveats ----
        tabPanel(
          "Sources & Caveats",
          h3("Data sources"),
          tags$ul(
            tags$li(lp_html(
              bold("Prison Policy Initiative (PPI): "),
              "Population counts for every year, state, and supervision type from 1978 to ", s$latest_year,
              ". This builds on, corrects, and extends existing BJS data — including harmonizing ",
              "definitions across states and years, and filling gaps where BJS data were missing, ",
              "inconsistent, or known to be inaccurate. Full details are in PPI's briefing: ",
              link_to("https://www.prisonpolicy.org/blog/2026/06/02/supervision/",
                      "Probation and parole trends by state: A look back at the data on 'alternatives' to incarceration"),
              ". The underlying spreadsheet is available ",
              link_to("https://www.prisonpolicy.org/data/incarceration_supervision_by_state_1978_2024.xlsx",
                      "from PPI"), ".")),
            tags$li(lp_html(
              bold("Bureau of Justice Statistics (BJS): "),
              "Admissions and exits come directly from the BJS Probation and Parole annual surveys and ",
              "are available for 2012 through ", s$bjs_year, ". Because of this, the State and National ",
              "Dashboards open on 2012 so that admissions, exits, and population cover the same period. ",
              "More at ", link_to("https://bjs.ojp.gov/", "bjs.ojp.gov"), "."))
          ),
          
          h3(id = "why_ppi", "Why population counts come from PPI instead of BJS"),
          div(class = "about-callout",
              p(lp_html("The Bureau of Justice Statistics (BJS) is the official federal source for probation and ",
                        "parole data, and this dashboard still uses it for admissions and exits. For ",
                        ital("population counts"), ", though, it uses the Prison Policy Initiative's (PPI) corrected ",
                        "series. PPI's series is not a separate survey: it starts from BJS's published files and ",
                        "checks them against state and local sources, including state agency data and public ",
                        "records. Where BJS's figures were missing, inconsistent, or contradicted by a state's ",
                        "own records, PPI replaced them, or removed them and interpolated between neighboring ",
                        "years. It made corrections to historical data for 21 states.")),
              p(lp_html("The full reasoning, state-by-state notes, and a table of which states were corrected are in ",
                        "PPI's report, ", link_to(ppi_report_url,
                                                  "Probation and parole trends by state: A look back at the data on 'alternatives' to incarceration"),
                        " (Jacob Kang-Brown, Prison Policy Initiative, June 2, 2026). The table below summarizes the ",
                        "main problems the report documents. The examples and figures in it come from PPI's report, ",
                        "not from this dashboard's own analysis."))),
          simple_table(ppi_vs_bjs_df),
          
          h3("What this means when you use the dashboard"),
          tags$ul(
            tags$li(strong("State trends are more trustworthy, not perfect. "),
                    "Using PPI's series avoids several false jumps and drops that appear in BJS's published ",
                    "figures. PPI itself says Alaska, Idaho, Nevada, and West Virginia remain uncertain; that ",
                    "Utah's figures for 2022 through 2024 are a clear underestimate (they cover only the state ",
                    "corrections department); that Georgia's probation rate is likely inflated by duplicate ",
                    "records for people with cases in more than one county; and that private and local ",
                    "probation coverage is still incomplete in several states."),
            tags$li(strong("Numbers here may not match BJS's tables. "),
                    "If you compare a state's probation or parole count in this dashboard with a BJS report, ",
                    "you may see differences. They can reflect PPI's corrections, and BJS also divides by adult ",
                    "residents rather than all residents when it reports rates."),
            tags$li(strong("Admissions and exits have not been corrected. "),
                    "PPI's state dataset covers population counts only, so admissions and exits still come ",
                    "straight from BJS (using BJS's imputed series). They may carry some of the same ",
                    "issues described above, such as imputed values and definition changes, and they will ",
                    "not always reconcile with the corrected population counts. Treat a sharp one-year change ",
                    "in a state's admissions or exits with caution."),
            tags$li(strong("Corrections can change over time. "),
                    "PPI invites updated data from states and researchers, so a future release of its ",
                    "workbook may revise figures shown here.")
          ),
          
          h3("Things to keep in mind"),
          tags$ul(
            tags$li(strong("Rates use all ages. "),
                    "Per-100,000 rates divide by the total resident population, including children. BJS's own ",
                    "published rates use adult residents, so they run higher than the rates shown here."),
            tags$li(strong("Counting dates differ. "),
                    "Community supervision and prison counts are generally year-end, while jail counts are ",
                    "generally mid-year. Comparing types in the same year therefore mixes reference dates."),
            tags$li(strong("Incarceration data have gaps. "),
                    "See the coverage table under Data Dictionary. In particular, jail data since 2020 cover ",
                    "only about half of states."),
            tags$li(strong("Admissions and exits can have missing state-years. "),
                    "Some BJS figures are imputed to account for non-reporting states and years, and a few ",
                    "states are missing outright (for example, Georgia and Michigan in 2016). A one-year dip ",
                    "in a national series may be a reporting artifact; check the Table and Data Source column."),
            tags$li(strong("People on both probation and parole are counted twice "),
                    "in the Community Supervision total."),
            tags$li(strong("Definitions vary across states. "),
                    "PPI harmonizes what it can, but states still differ in how they organize probation, ",
                    "parole, jails, and prisons."),
            tags$li(strong("The federal system is separate. "),
                    "It is included in National Dashboard totals, but left out of state means and medians and ",
                    "of per-capita comparisons."),
            tags$li(strong("Missing data on the map look like zeros. "),
                    "States with no data for the current filters are drawn in the lightest shade; check the ",
                    "Table to tell a true zero from missing data.")
          ),
          p(class = "reporting-note",
            "Where PPI and BJS overlap, this dashboard combines them to maximize year and state coverage. ",
            "The Table on the Main Dashboard always shows the underlying values behind any chart.")
        )
      )
  )
)


####App####
##UI
ui <- navbarPage("National Supervision Data Dashboard",
                 id = "main_nav",
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
      }")),
      tags$style(HTML(landing_css)),
      tags$script(HTML("Shiny.addCustomMessageHandler('scrollToTop', function(x) { window.scrollTo({top: 0, behavior: 'smooth'}); });"))),
      
      home_tab,
      
      about_tab,
      
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
  
  ####Home page####
  # Buttons on the Home page jump to the other tabs
  nav_targets <- c(go_main = "Main Dashboard", go_state = "State Dashboard",
                   go_national = "National Dashboard", go_about = "about",
                   next_main = "Main Dashboard", next_state = "State Dashboard",
                   next_national = "National Dashboard", next_about = "about")
  
  lapply(names(nav_targets), function(btn) {
    observeEvent(input[[btn]], {
      updateNavbarPage(session, "main_nav", selected = nav_targets[[btn]])
      session$sendCustomMessage("scrollToTop", TRUE)
    }, ignoreInit = TRUE)
  })
  
  # "Why PPI instead of BJS?" link on the Home page: open About > Sources & Caveats
  observeEvent(input$why_ppi_more, {
    updateNavbarPage(session, "main_nav", selected = "about")
    updateTabsetPanel(session, "about_tabs", selected = "Sources & Caveats")
    session$sendCustomMessage("scrollToTop", TRUE)
  }, ignoreInit = TRUE)
  
  # Shared chart styling for the Home page
  lp_font  <- list(family = "Roboto, sans-serif", size = 12, color = "#2C2C2C")
  lp_grid  <- "#E6DDC6"
  lp_surface <- "#FDF6E3"
  
  # Width of a rendered plot in pixels (used to drop end-of-line labels on narrow screens)
  plot_width <- function(id) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(w)) 900 else w
  }
  
  # Trend 1: community supervision, probation, and parole over time
  output$landing_trend_plot <- renderPlotly({
    use_rate <- identical(input$landing_metric, "rate")
    wide <- plot_width("landing_trend_plot") >= 640
    
    df <- landing_trend %>% mutate(value = if (use_rate) rate else count)
    fmt_val <- function(x) {
      if (use_rate) fmt_count(x) else paste0(formatC(x / 1e6, format = "f", digits = 2), "M")
    }
    
    p <- plot_ly()
    for (tp in supervision_types) {
      p <- p %>%
        add_trace(
          data = df %>% filter(control_type == tp),
          x = ~year, y = ~value, type = "scatter", mode = "lines",
          name = tp, line = list(color = supervision_colors[[tp]], width = 2),
          hovertemplate = paste0("%{y:,.0f}", if (use_rate) " per 100,000" else "", "<extra>", tp, "</extra>")
        )
    }
    
    pk <- df %>% filter(control_type == "Community Supervision") %>% slice_max(value, n = 1, with_ties = FALSE)
    p <- p %>%
      add_trace(
        data = pk, x = ~year, y = ~value, type = "scatter", mode = "markers",
        showlegend = FALSE, hoverinfo = "skip",
        marker = list(color = supervision_colors[["Community Supervision"]], size = 9,
                      line = list(color = lp_surface, width = 2))
      )
    
    annotations <- list(
      list(x = pk$year, y = pk$value, showarrow = FALSE, yshift = 16, font = list(size = 11),
           text = paste0("Peak: ", fmt_val(pk$value), " (", pk$year, ")"))
    )
    if (wide) {
      annotations <- c(annotations, lapply(supervision_types, function(tp) {
        d <- df %>% filter(control_type == tp, year == max(year))
        list(x = d$year, y = d$value, showarrow = FALSE, xanchor = "left", xshift = 8,
             font = list(size = 11),
             text = paste0(str_to_sentence(tp), " <b>", fmt_val(d$value), "</b>"))
      }))
    }
    
    p %>%
      layout(
        font = lp_font, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        hovermode = "x unified",
        xaxis = list(title = "", tickformat = "d", hoverformat = "d", dtick = 10, tick0 = 1980,
                     range = c(landing_stats$first_year - 1, landing_stats$latest_year + 1),
                     showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = list(text = if (use_rate) "People per 100,000 residents" else "People under supervision",
                                  font = list(size = 12)),
                     tickformat = if (use_rate) ",d" else "~s",
                     range = c(0, max(df$value) * 1.12),
                     gridcolor = lp_grid, zeroline = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"),
        annotations = annotations,
        margin = list(l = 60, r = if (wide) 215 else 20, t = 50, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Trend 2: probation vs. parole, percent change since the base year
  output$landing_change_plot <- renderPlotly({
    wide <- plot_width("landing_change_plot") >= 640
    base <- landing_stats$base_year
    last_year <- landing_stats$latest_year
    df <- landing_change
    
    # Y range: a little headroom above 0% and below the lowest value, in 5-point steps
    ymin <- floor((min(df$pct_change) - 0.01) / 0.05) * 0.05
    ymax <- max(0.03, ceiling((max(df$pct_change) + 0.02) / 0.05) * 0.05)
    tick_vals  <- round(seq(ymin, ymax, by = 0.05), 2)
    tick_label <- ifelse(tick_vals > 0, "+", ifelse(tick_vals < 0, "−", "")) %>%
      paste0(formatC(abs(tick_vals) * 100, format = "f", digits = 0), "%")
    
    p <- plot_ly()
    for (tp in c("Probation", "Parole")) {
      p <- p %>%
        add_trace(
          data = df %>% filter(control_type == tp),
          x = ~year, y = ~pct_change, customdata = ~count,
          type = "scatter", mode = "lines+markers", name = tp,
          line = list(color = supervision_colors[[tp]], width = 2),
          marker = list(color = supervision_colors[[tp]], size = 8,
                        line = list(color = lp_surface, width = 2)),
          hovertemplate = paste0("%{y:+.1%} vs. ", base, "  (%{customdata:,.0f} people)<extra>", tp, "</extra>")
        )
    }
    
    # The 0% line is the reference: "same as the base year"
    annotations <- list(
      list(x = base, y = 0, showarrow = FALSE, xanchor = "left", yanchor = "bottom", yshift = 4,
           font = list(size = 11, color = "#4A4A4A"),
           text = paste0("0% = same as ", base))
    )
    if (wide) {
      annotations <- c(annotations, lapply(c("Probation", "Parole"), function(tp) {
        d <- df %>% filter(control_type == tp, year == last_year)
        list(x = d$year, y = d$pct_change, showarrow = FALSE, xanchor = "left", xshift = 10,
             font = list(size = 11),
             text = paste0(tp, " <b>", ifelse(d$pct_change >= 0, "+", "−"), fmt_pct(d$pct_change), "</b>"))
      }))
    }
    
    p %>%
      layout(
        font = lp_font, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        hovermode = "x unified",
        xaxis = list(title = "", tickformat = "d", hoverformat = "d", dtick = 1,
                     range = c(base - 0.3, last_year + 0.3), showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = list(text = paste0("Percent change since ", base), font = list(size = 12)),
                     tickmode = "array", tickvals = tick_vals, ticktext = tick_label,
                     range = c(ymin, ymax), gridcolor = lp_grid, zeroline = FALSE),
        shapes = list(list(type = "line", x0 = base - 0.3, x1 = last_year + 0.3, y0 = 0, y1 = 0,
                           line = list(color = "#6B6B6B", width = 1.5))),
        legend = list(orientation = "h", x = 0, y = 1.12, yanchor = "bottom"),
        annotations = annotations,
        margin = list(l = 60, r = if (wide) 130 else 20, t = 50, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Trend 3: highest and lowest state rates
  output$landing_states_plot <- renderPlotly({
    df <- landing_state_bars
    nat <- landing_stats$cs_latest_rate
    
    # Bar color follows the rate: a one-hue ramp from the Community Supervision
    # orange (lowest rate shown) to a dark burnt orange (highest). Interpolated in
    # Lab so the steps look even; the light end still clears 2:1 against the page.
    rate_range <- range(df$rate, na.rm = TRUE)
    is_spacer  <- is.na(df$rate)
    df <- df %>%
      mutate(
        bar_color = scales::seq_gradient_pal("#E69F00", "#8C3D00", "Lab")(
          (rate - rate_range[1]) / diff(rate_range)),
        # The blank spacer row is a zero-length, transparent bar with no label or tooltip.
        # It is kept as a real (0) value rather than NA so that plotly keeps the bars,
        # colors, and labels lined up row by row.
        bar_color = ifelse(is_spacer, "rgba(0,0,0,0)", bar_color),
        bar_value = ifelse(is_spacer, 0, rate),
        bar_label = ifelse(is_spacer, "", fmt_count(rate)),
        bar_hover = ifelse(is_spacer, "",
                           paste0("<b>", state_name, "</b><br>", fmt_count(rate),
                                  " per 100,000<br>", fmt_count(count), " people")))
    
    p <- plot_ly(
      df, x = ~bar_value, y = ~state_name, type = "bar", orientation = "h",
      marker = list(color = ~bar_color),
      text = ~bar_label, textposition = "outside", cliponaxis = FALSE,
      textfont = list(size = 11, color = "#2C2C2C"),
      hovertext = ~bar_hover, hoverinfo = "text"
    ) %>%
      layout(
        font = lp_font, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE, bargap = 0.35,
        xaxis = list(title = list(text = "People on community supervision per 100,000 residents",
                                  font = list(size = 12)),
                     tickformat = ",d", range = c(0, max(df$rate, na.rm = TRUE) * 1.15),
                     gridcolor = lp_grid, zeroline = FALSE),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(df$state_name),
                     automargin = TRUE),
        shapes = list(list(type = "line", x0 = nat, x1 = nat, y0 = 0, y1 = 1, yref = "paper",
                           line = list(color = "#2C2C2C", width = 1))),
        annotations = list(list(x = nat, y = 1, yref = "paper", yanchor = "bottom", xanchor = "left",
                                showarrow = FALSE, font = list(size = 11),
                                text = paste0("U.S. overall: <b>", fmt_count(nat), "</b>"))),
        margin = list(l = 10, r = 30, t = 30, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  ####About page####
  output$glossary_table <- renderDT({
    dict_datatable(glossary_df, widths = c("17%", "20%", "63%"))
  })
  
  output$data_dictionary_table <- renderDT({
    dict_datatable(fields_df, widths = c("13%", "17%", "8%", "27%", "35%"), filter = "none")
  })
  
  output$coverage_table <- renderDT({
    dict_datatable(coverage_df, widths = c("20%", "11%", "13%", "56%"), filter = "none")
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