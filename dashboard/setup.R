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
#library(reactable)
#library(reporter)

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
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 
                     'zoomOut2d', 'autoScale2d', 
                     'toggleSpikelines', 
                     'hoverCompareCartesian', 
                     'hoverClosestCartesian', 'toImage')
# LOAD IN DATA ----

source(here("dashboard/data_setup/data_load_shiny.R"))

# selections lists

revised_year_sup <- paste0(revised_year,"ᴿ")

# revised_year_sup <- revised_year %p% supsc("R")

provisional_year_sup <- paste0(provisional_year,"ᴾ")

included_years_no_sup <- c(finalised_years, provisional_year)

included_years <- c(finalised_years, provisional_year_sup)

included_years_sup <- c(finalised_years[-length(finalised_years)], revised_year_sup, provisional_year_sup)

#included_years <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

home_list <- c("About" = "about",
               "Using the Dashboard" = "use",
               "Further Information" = "info",
               #"Data Definitions" = "defs",
               "Accessibility" = "access")

# trend_list <- c("Number of referrals" = "referrals_trend",
#                 "Percentage receiving one year's support" = "pds_perc_trend",
#                 "Percentage of estimated diagnoses referred" = "exp_perc_trend")

# data_list <- c("Subtype of Dementia" = "data_subtype",
#                "Stage of Dementia" = "data_stage",
#                "PDS Referral Source" = "data_referral",
#                "Model of Care" = "data_model",
#                "PDS Uptake" = "uptake",
#                "PDS Pathways" = "waiting_times"

ldp_tab_list <- c("LDP Standard Part 1" = "ldp_part_1",
              "LDP Standard Part 2" = "ldp_part_2"
                 )

ldp_sidebar_list <- c("Outcomes by Financial Year" = "outcomes",
                      "Trends" = "trends"
                      )

demographics_list <- c("Gender" = "data_sex",
                       "Age" = "data_age",
                       "Deprivation (SIMD)" = "data_simd")
                      # "Accommodation" = "data_accom") 

pathways_list <- c("Waiting Times by Financial Year" = "wait",
                      "Trends" = "trends"
)

method_list <- c("LDP Classification" = "ldp_class",
                       "Number of Expected Diagnoses" = "exp_diag",
                       "Removal of Duplicate Records" = "duplicates") 

download_list <- c("Scotland" = "download_data_scotland",
                  "Health Boards" = "download_data_hb",
                  "Integration Authority Areas" = "download_data_ijb")


boards <- as.character(sort(unique(filter(annual_table_data, health_board != "Scotland")$health_board)))
ijb_list <- as.character(sort(unique(filter(annual_table_data, ijb != "Scotland", !grepl("NHS", ijb))$ijb)))

simd_list <- as.character(sort(unique(filter(data_simd, type != "Unknown")$type)))


### END OF SCRIPT----