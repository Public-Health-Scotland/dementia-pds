#########################################################################
# Name of file - 00_setup-environment.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Sets up environment required for running quarterly management
# reports. This is the only file which should require updating everytime 
# the process is run.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Load packages ----

library(dplyr)         # For data manipulation in the "tidy" way
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidylog)       # For printing results of some dplyr functions
library(tidyr)         # For data manipulation in the "tidy" way
library(stringr)       # For string manipulation and matching
library(here)          # For the here() function
library(glue)          # For working with strings
library(ggplot2)       # For plotting
library(plotly)        # For interactive plots
library(flexdashboard) # For creating markdown outputs
library(purrr)         # For functional programming
library(fs)            # For creating folder directories
library(forcats)       # For factor manipulation
library(knitr)         # For creating kable tables
library(kableExtra)    # For customising kable tables
library(phsmethods)    # For formatting postcode


### 2 - Define Whether Running on Server or Locally ----

if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
  platform <- "server"
} else {
  platform <- "locally"
}


# Define root directory for stats server based on whether script is running 
# locally or on server
filepath <- dplyr::if_else(platform == "server",
                           "/conf/",
                           "//stats/")


### 3 - Extract dates ----

# Define the dates that the data are extracted from and to

# Start date of reporting period
# Update annually when Q1 of new FY is submitted
start_date <- lubridate::ymd(20160401)

# End date of reporting period
# Update quarterly to last day of new quarter
end_date   <- lubridate::ymd(20190930)

# FY and Quarter of reporting period
fy         <- "2019"
qt         <- "2"       


### 4 - Disable scientific notation ----

options(scipen=999)


### 5 - Set knitr options ----

# Allow duplicate labels
options(knitr.duplicate.label = 'allow')

# Knitr hook to add thousands separator
# knit_hooks$set(inline = function(x) {
#   prettyNum(x, big.mark=",")
# })


### 6 - Define exempt termination reason codes ----

exempt_reasons <- c("03", "04", "05", "06")


### 7 - SIMD Lookup ----

simd     <- function(){
  
  read_rds(glue("{filepath}/linkage/output/lookups/Unicode/",
                "Deprivation/postcode_2019_2_simd2016.rds")) %>%
    
    clean_names() %>%
    
    select(pc7, simd2016_sc_quintile) %>%
    
    rename(simd = simd2016_sc_quintile) %>%
    
    mutate(
      simd = case_when(
        simd == 1 ~ "1 - Most Deprived",
        simd == 5 ~ "5 - Least Deprived",
        TRUE ~ as.character(simd)
      )
    )
  
}


### END OF SCRIPT ###