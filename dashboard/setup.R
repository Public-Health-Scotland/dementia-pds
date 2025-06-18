####################### Setup #######################
# load setup environment
source(here::here("code", "00_setup-environment.R"))

# Shiny packages ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(bslib)
library(DT)

# Data wrangling packages ----
library(dplyr)
library(tidyr)
library(magrittr)

# Plotting packages ----
library(plotly)

# PHS styling packages ----
library(phsstyles)

# Load functions ----
source(here("dashboard/functions/core_functions.R"))
source(here("dashboard/functions/plot_functions.R"))

## Plotting ----
# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove from plotly plots
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')
# LOAD IN DATA ----

source(here("dashboard/data_setup/data_load_shiny.R"))

# selections lists

provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
                           "/", as.numeric(substr(last(finalised_years),6,7)) + 1)

#included_years <- c(finalised_years, provisional_year)

included_years <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

home_list <- c("Dementia PDS" = "about",
               "Using the Dashboard" = "use",
               "Further Information" = "info",
               #"Data Definitions" = "defs",
               "Accessibility" = "accessibility")

trend_list <- c("Number of referrals" = "referrals_trend",
                "Percentage of LDP standard achieved" = "pds_perc_trend",
                "Percentage of estimated diagnoses referred" = "exp_perc_trend")

data_list <- c("Subtype of Dementia" = "data_subtype",
               "Stage of Dementia" = "data_stage",
               "PDS Referral Source" = "data_referral",
               "Model of Care" = "data_model",
               "PDS Uptake" = "uptake",
               "PDS Pathways" = "waiting_times"
)

demographics_list <- c("Gender" = "data_sex",
                       "Age" = "data_age",
                       "SIMD" = "data_simd",
                       "Accommodation" = "data_accom") 

method_list <- c("Local Delivery Plan (LDP) Classification" = "ldp_class",
                       "Number of Expected Diagnoses" = "exp_diag",
                       "Removal of Duplicate Records" = "duplicates") 

quality_list <- c("Queries/Errors" = "errors",
                  "Number of Records Submitted" = "records") 


boards <- sort(unique(pds_plot_data$health_board))
ijb_list <- as.character(sort(unique(filter(err, ijb != "" & ijb != "Unknown")$ijb)))

simd_list <- as.character(sort(unique(filter(data_simd, type != "Unknown")$type)))





