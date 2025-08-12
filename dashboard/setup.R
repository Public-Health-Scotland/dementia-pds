####################### Setup #######################
# load setup environment
source(here::here("code", "publication", "00_setup-pub-environment.R"))

# Shiny packages ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(bslib)
library(DT)

# PHS styling packages ----
library(phsstyles)

# Load functions ----
source(here("dashboard/functions/core_functions.R"))
source(here("dashboard/functions/plot_functions.R"))

# LOAD IN DATA ----

source(here("dashboard/data_setup/data_load_shiny.R"))

# selections lists

#adds superscript R to year which was provisional in previous publication
revised_year_sup <- paste0(revised_year,"ᴿ")

#adds superscript P to provisional data
provisional_year_sup <- paste0(provisional_year,"ᴾ")

extra_referrals_year_sup <- paste0(extra_referrals_year,"ᴾ")

# year list with no superscript R for data that was not included in 2024 publication
included_years_sup <- c(finalised_years, provisional_year_sup) #REMOVE from 2026 onward

#list of included years for ldp standard, demographics and pathways data
included_years <- c(finalised_years[-length(finalised_years)],
                        revised_year_sup,
                        provisional_year_sup)

#list of included years for referrals and rates page
included_years_extra_referrals <- c(finalised_years[-length(finalised_years)], revised_year_sup, provisional_year_sup, extra_referrals_year_sup)


#sidebar buttons for home page
home_list <- c("About" = "about",
               "Using the Dashboard" = "use",
               "Further Information" = "info",
               #"Data Definitions" = "defs",
               "Accessibility" = "access")


#tabs for ldp standard page
ldp_tab_list <- c("LDP Standard Part 1" = "ldp_part_1",
              "LDP Standard Part 2" = "ldp_part_2"
                 )

#sidebar buttons for ldp standard page
ldp_sidebar_list <- c("Outcomes by Financial Year" = "outcomes",
                      "Trends" = "trends"
                      )

#tabs for rates and referrals page
RandR_tab_list <- c("Total Referrals" = "RandR_totals",
                    "Rates per 10,000 population" = "RandR_rates"
)

#sidebar buttons for rates and referrals page
RandR_sidebar_list <- c("Referrals by Financial Year" = "referrals",
                        "Trends" = "trends"
)

#sidebar buttons for demographics page
demographics_list <- c("Gender" = "data_sex",
                       "Age" = "data_age",
                       "Deprivation (SIMD)" = "data_simd")

#sidebar buttons for pathways page
pathways_list <- c("Waiting Times by Financial Year" = "wait",
                      "Trends" = "trends"
)

#tabs for methodology page
method_list <- c("LDP Classification" = "ldp_class",
                       "Number of Expected Diagnoses" = "exp_diag",
                       "Removal of Duplicate Records" = "duplicates") 

#dropdown selection for download page
download_list <- c("Scotland" = "download_data_scotland",
                  "Health Boards" = "download_data_hb",
                  "Integration Authority Areas" = "download_data_ijb")

#list of health boards and integration authority areas
boards <- as.character(sort(unique(filter(annual_table_data, health_board != "Scotland")$health_board)))
ijb_list <- as.character(sort(unique(filter(annual_table_data, ijb != "Scotland", !grepl("NHS", ijb))$ijb)))

simd_list <- as.character(sort(unique(filter(data_simd, type != "Unknown")$type)))


### END OF SCRIPT----