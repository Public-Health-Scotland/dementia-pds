################################################################################.
# Name of file - setup.R
# Original Authors - Zaineb
# Updated by Abram McCormick - Sep 2024
# Updated by Lucy Binsted - Jan 2026
# Written/run on - RStudio Server
# Version of R - 4.4.2
# Description - Setup for shiny dashboard.
#               NOTE: finalised_years, finalised_years_demographics and finalised_years_extra_referrals are defined in code/00_setup-environment.R
################################################################################.

################################################################################.
# Load packages ----
################################################################################.

# Shiny packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(bslib)
library(DT)
library(shinymanager)

# Other packages
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(english)
library(readxl)
library(plotly)
library(stringr)
library(purrr)
library(magrittr)

# PHS styling packages
library(phsstyles)

################################################################################.
# Load functions ----
################################################################################.

source("functions/core_functions.R")
source("functions/plot_functions.R")

################################################################################.
# Load data ----
################################################################################.

load("data/dashboard_variables.RData")
annual_table_data <- read_rds("data/annual_table_data.rds")
data_wait <- read_rds("data/data_wait.rds")
data_age <- read_rds("data/data_age.rds")
data_simd <- read_rds("data/data_simd.rds")
data_sex <- read_rds("data/data_sex.rds")
data_rates <- read_rds("data/data_rates.rds")
download_data_scotland <- read_rds("data/download_data_scotland.rds")
download_data_hb <- read_rds("data/download_data_hb.rds")
download_data_ijb <- read_rds("data/download_data_ijb.rds")

################################################################################.
# Define provisional and revised years (with and without superscripts) ----
################################################################################.

# Helper function to take the last year in finalised_years (YYYY/YY) and return the year-range advanced by modifier (e.g. "2023/24" -> "2024/25")
create_new_year <- function(finalised_years, modifier){
  return(paste0(as.numeric(substr(last(finalised_years),1,4)) + modifier,
                "/", as.numeric(substr(last(finalised_years),6,7)) + modifier))}

# Provisional - the two years after the last finalised year
provisional_year <- create_new_year(finalised_years, 1)
extra_referrals_year <- create_new_year(finalised_years, 2)

# Revised - last finalised year
revised_year <- create_new_year(finalised_years, 0)

# Add superscripts
provisional_year_sup <- paste0(provisional_year,"ᴾ")
extra_referrals_year_sup <- paste0(extra_referrals_year,"ᴾ")
revised_year_sup <- paste0(revised_year,"ᴿ")

################################################################################.
# Define year lists ----
################################################################################.

# Helper function to take the finalised_years list, add superscripts to revised years and add provisional years with superscripts
create_year_list <- function(finalised_years, revised_years, provisional_years) {
  years <- as.character(finalised_years)
  years[years %in% revised_years] <- paste0(years[years %in% revised_years], "ᴿ")
  c(years, paste0(provisional_years, "ᴾ"))
}

# Years for LDP Standard (finalised + one provisional)
included_years <- create_year_list(
  finalised_years, 
  revised_years = c(revised_year), 
  provisional_years = c(provisional_year))

# Years for Referrals (finalised + two provisional)
included_years_extra_referrals <- create_year_list(
  finalised_years, 
  revised_years = c(revised_year), 
  provisional_years = c(provisional_year, extra_referrals_year))

################################################################################.
# Add superscripts to data frames ----
################################################################################.

# Lookup
superscript_lookup <- setNames(included_years_extra_referrals, gsub("[ᴾᴿ]", "", included_years_extra_referrals))

# Download data
for (df_name in c("download_data_scotland", "download_data_hb", "download_data_ijb")) {
  assign(df_name, get(df_name) %>% 
           mutate(financial_year = coalesce(superscript_lookup[financial_year], financial_year)))
}

# LDP, Pathways, Demographics and Rates data
for (df_name in c("annual_table_data", "data_wait", "data_age", "data_sex", "data_simd", "data_rates")) {
  assign(df_name, get(df_name) %>% 
           mutate(fy = coalesce(superscript_lookup[fy], fy)))
}

################################################################################.
# Set factor levels in data frames ----
################################################################################.

# Helper function to set health board, ijb, and other specified columns as factors
set_factors <- function(data, cols, custom_levels = list()) {
  data$ijb <- factor(data$ijb, levels = unique(annual_table_data$ijb))
  data$health_board <- factor(data$health_board, levels = unique(annual_table_data$health_board))
  for (col in cols) {
    if (col %in% names(custom_levels)) {
      levels <- custom_levels[[col]] # Use custom levels if specified
    } else {
      levels <- sort(unique(data[[col]])) # Default: alphabetical order of unique values
    }
    data[[col]] <- factor(data[[col]], levels = levels)
  }
  return(data)
}

annual_table_data <- set_factors(annual_table_data, cols = c("ldp", "fy"))
data_wait <- set_factors(data_wait, cols = "fy")
data_age <- set_factors(data_age, cols = c("type", "fy"))
data_sex <- set_factors(data_sex, cols = c("type", "fy"), custom_levels = list(type = c("Male", "Female", "Not Specified", "Unknown")))
data_simd <- set_factors(data_simd, cols = c("type", "fy"))
data_rates <- set_factors(data_rates, cols = "fy")

################################################################################.
# Create data frame for sex with Scotland totals and no unknowns ----
################################################################################.

all_data_sex <- data_sex %>% 
  group_by(health_board, ijb, fy) %>% 
  summarize(
    type = "Scotland", referrals = sum(referrals), complete = sum(complete), exempt = sum(exempt), 
    ongoing = sum(ongoing), not_met = sum(not_met), percent_met = (sum(complete+exempt)/referrals)) %>%
  merge(data_sex %>% filter(!(type %in% c("Not Specified", "Unknown"))), all = T) %>% 
  mutate(type = factor(type, levels = c("Male", "Female","Scotland")))

################################################################################.
# Define buttons, tabs and dropdowns ----
################################################################################.

# Sidebar buttons for home page
home_list <- c(
  "About" = "about",
  "Using the Dashboard" = "use",
  "Glossary" = "glossary",
  "Accessibility" = "access",
  "Contact" = "contact")

# Tabs for ldp standard page
ldp_tab_list <- c(
  "LDP Standard Part 1" = "ldp_part_1",
  "LDP Standard Part 2" = "ldp_part_2")

# Sidebar buttons for ldp standard page
ldp_sidebar_list <- c(
  "Outcomes by Financial Year" = "outcomes",
  "Trends" = "trends")

# Tabs for rates and referrals page
RandR_tab_list <- c(
  "Total Referrals" = "RandR_totals",
  "Rates per 10,000 Population" = "RandR_rates")

# Sidebar buttons for rates and referrals page
RandR_sidebar_list <- c(
  "Referrals by Financial Year" = "referrals",
  "Trends" = "trends")

# Sidebar buttons for demographics page
demographics_list <- c(
  "Sex" = "data_sex",
  "Age" = "data_age",
  "Deprivation (SIMD)" = "data_simd")

# Sidebar buttons for pathways page
pathways_list <- c(
  "Time to first contact by Financial Year" = "wait",
  "Trends" = "trends")

# Tabs for methodology page
method_list <- c(
  "LDP Classification" = "ldp_class",
  "Number of Expected Diagnoses" = "exp_diag",
  "Removal of Duplicate Records" = "duplicates") 

# Dropdown selection for download page
download_list <- c(
  "Scotland" = "download_data_scotland",
  "Health Boards" = "download_data_hb",
  "Integration Authority Areas" = "download_data_ijb")

################################################################################.
# List of health boards and integration authority areas ----
################################################################################.

boards <- as.character(sort(unique(filter(annual_table_data, health_board != "Scotland")$health_board)))
ijb_list <- as.character(sort(unique(filter(annual_table_data, ijb != "Scotland", !grepl("NHS", ijb))$ijb)))

### END OF SCRIPT----