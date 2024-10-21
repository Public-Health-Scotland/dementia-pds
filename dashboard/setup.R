####################### Setup #######################


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

# Load core functions ----
source(here("dashboard/functions/core_functions.R"))

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

source(here("dashboard/data/data_load_shiny.R"))

# selections lists
provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
                           "/", as.numeric(substr(last(finalised_years),6,7)) + 1)

included_years <- c(finalised_years, provisional_year)

home_list <- c("Dementia PDS" = "about",
               "Using the Dashboard" = "use",
               "Further Information" = "info",
               #"Data Definitions" = "defs",
               "Accessibility" = "accessibility")

data_list <- c("Subtype of Dementia" = "data_subtype",
               "Stage of Dementia" = "data_stage",
               "PDS Referral Source" = "data_referral",
               "Model of Care" = "data_model",
               "Waiting Times" = "waiting_times"
)

demographics_list <- c("Age" = "data_age",
                       "SIMD" = "data_simd",
                       "Accommodation" = "data_accom") #include accommodation type and living alone status


boards <- sort(unique(pds_plot_data$health_board))
ijb_list <- as.character(sort(unique(filter(data_wait, ijb != "All")$ijb)))





