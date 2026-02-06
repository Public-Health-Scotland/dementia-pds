################################################################################
# Name of file - setup.R
# Original Authors - Zaineb
# Updated by Abram McCormick - Sep 2024
# Updated by Lucy Binsted - Jan 2026
# Written/run on - RStudio Server
# Version of R - 4.4.2
# Description - Setup for shiny dashboard 
################################################################################

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
# Define provisional and revised years ----
################################################################################.

# Helper function to take the last year in finalised_years (YYYY/YY) and return the year-range advanced by modifier (e.g. "2023/24" -> "2024/25")
create_new_year <- function(finalised_years, modifier){
  return(paste0(as.numeric(substr(last(finalised_years),1,4)) + modifier,
                "/", as.numeric(substr(last(finalised_years),6,7)) + modifier))}

# Provisional
provisional_year <- create_new_year(finalised_years, 1)
extra_referrals_year <- create_new_year(finalised_years, 2)

provisional_year_sup <- paste0(provisional_year,"ᴾ")
extra_referrals_year_sup <- paste0(extra_referrals_year,"ᴾ")

# Revised
revised_year <- create_new_year(finalised_years, 0)
revised_year_extra <- create_new_year(finalised_years, -1)
revised_year_extra_2 <- create_new_year(finalised_years, -2)

revised_year_sup <- paste0(revised_year,"ᴿ")
revised_year_sup_extra <- paste0(revised_year_extra,"ᴿ")
revised_year_sup_extra_2 <- paste0(revised_year_extra_2,"ᴿ")

################################################################################.
# Define included years ----
################################################################################.

# Year list with no superscript R for data that was not included in 2024 publication
included_years_2025_gender_wait <- c(finalised_years, provisional_year_sup) # Remove from 2026 onward ----

# Year list with no superscript R for data that was not included in 2024 publication
included_years_pathways <- c(finalised_years, provisional_year_sup, extra_referrals_year_sup) # Remove from 2026 onward ----

# List of included years for ldp standard, demographics and pathways data
included_years <- c(finalised_years_referrals, revised_year_sup_extra, revised_year_sup, provisional_year_sup)

# List of included years for demographics data
included_years_demographics <- c(finalised_years_demographics, revised_year_sup_extra_2, revised_year_sup_extra, revised_year_sup, provisional_year_sup)

# List of included years for referrals and rates page
included_years_extra_referrals <- c(finalised_years_referrals, revised_year_sup_extra, revised_year_sup,provisional_year_sup, extra_referrals_year_sup)

# List of included years for referrals and rates page
included_years_extra_referrals_2025_rates <- c(finalised_years_referrals, provisional_year_sup, extra_referrals_year_sup) #REMOVE from 2026 onward----

################################################################################.
# Add superscripts to data frames ----
################################################################################.

# Helper function to add superscript "ᴾ" or "ᴿ" to fy column
add_superscripts <- function(data, fy_label, years){
  for (year in years){
    if (year %in% c("provisional_year", "extra_referrals_year")){
      data <- data %>% mutate(!!sym(fy_label) := ifelse(.data[[fy_label]] == get(year), paste0(get(year), "ᴾ"), as.character(.data[[fy_label]])))
    }
    if (year %in% c("revised_year", "revised_year_extra", "revised_year_extra_2")){
      data <- data %>% mutate(!!sym(fy_label) := ifelse(.data[[fy_label]] == get(year), paste0(get(year),"ᴿ"), as.character(.data[[fy_label]])))
    }
  }
  return(data)
}

years <- c("provisional_year", "extra_referrals_year", "revised_year", "revised_year_extra", "revised_year_extra_2")

download_data_scotland <- add_superscripts(download_data_scotland, "financial_year", years[1:4])
download_data_hb <- add_superscripts(download_data_hb, "financial_year", years[1:4])
download_data_ijb <- add_superscripts(download_data_ijb, "financial_year", years[1:4])
annual_table_data <- add_superscripts(annual_table_data, "fy", years[1:4])
data_wait <- add_superscripts(data_wait, "fy", years[1:2]) # Add "revised_year" for 2026 ----
data_age <- add_superscripts(data_age, "fy", years)
data_sex <- add_superscripts(data_sex, "fy", years)
data_simd <- add_superscripts(data_simd, "fy", years[c(1, 3, 4, 5)])
data_rates <- add_superscripts(data_rates, "fy", years[1:4]) # Remove "revised_year" for 2026 ----

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
  summarize(type = "Scotland", referrals = sum(referrals), complete = sum(complete), exempt = sum(exempt), 
            ongoing = sum(ongoing), not_met = sum(not_met), percent_met = (sum(complete+exempt)/referrals)) %>%
  merge(data_sex %>% filter(!(type %in% c("Not Specified", "Unknown"))), all = T) %>% 
  mutate(type = factor(type, levels = c("Male", "Female","Scotland")))

################################################################################.
# Define buttons, tabs and dropdowns ----
################################################################################.

# Sidebar buttons for home page
home_list <- c("About" = "about",
               "Using the Dashboard" = "use",
               "Glossary" = "glossary",
               "Accessibility" = "access",
               "Contact" = "contact")

# Tabs for ldp standard page
ldp_tab_list <- c("LDP Standard Part 1" = "ldp_part_1",
                  "LDP Standard Part 2" = "ldp_part_2")

# Sidebar buttons for ldp standard page
ldp_sidebar_list <- c("Outcomes by Financial Year" = "outcomes",
                      "Trends" = "trends")

# Tabs for rates and referrals page
RandR_tab_list <- c("Total Referrals" = "RandR_totals",
                    "Rates per 10,000 Population" = "RandR_rates")

# Sidebar buttons for rates and referrals page
RandR_sidebar_list <- c("Referrals by Financial Year" = "referrals",
                        "Trends" = "trends")

# Sidebar buttons for demographics page
demographics_list <- c("Gender" = "data_sex",
                       "Age" = "data_age",
                       "Deprivation (SIMD)" = "data_simd")

# Sidebar buttons for pathways page
pathways_list <- c("Time to first contact by Financial Year" = "wait",
                   "Trends" = "trends")

# Tabs for methodology page
method_list <- c("LDP Classification" = "ldp_class",
                 "Number of Expected Diagnoses" = "exp_diag",
                 "Removal of Duplicate Records" = "duplicates") 

# Dropdown selection for download page
download_list <- c("Scotland" = "download_data_scotland",
                   "Health Boards" = "download_data_hb",
                   "Integration Authority Areas" = "download_data_ijb")

################################################################################.
# List of health boards and integration authority areas ----
################################################################################.

boards <- as.character(sort(unique(filter(annual_table_data, health_board != "Scotland")$health_board)))
ijb_list <- as.character(sort(unique(filter(annual_table_data, ijb != "Scotland", !grepl("NHS", ijb))$ijb)))

### END OF SCRIPT----