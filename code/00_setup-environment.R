#########################################################################
# Name of file - 00_setup-environment.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Sets up environment required for running quarterly management
# reports. This is the only file which should require updating everytime 
# the process is run.
#########################################################################


### 0 - UPDATE THIS DATE ###

# Last day in reporting period
end_date   <- lubridate::ymd(20191231)

# Date of publication - Only used when running publication, can comment out otherwise
pub_date <- lubridate::ymd(20200331)
fy_in_pub <- c("2016/17", "2017/18")


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
library(openxlsx)      # For working with Excel files
library(flextable)     # For formatted tables in publication output
library(usethis)       # For creating folder structure


### 2 - Define file paths dependent on whether running on server or desktop ----

stats <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ "/conf",
  TRUE ~ "//stats"
)

cl_out <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ 
    "/conf/linkage/output",
  TRUE ~ "//stats/cl-out"
)


### 3 - Extract dates ----

# Start date of reporting period
start_date <- lubridate::ymd(
  paste0(if_else(month(end_date) >= 4,
                 year(end_date) - 3,
                 year(end_date) - 4),
         "0401")
)

# FY and Quarter of reporting period
fy <- phsmethods::fin_year(end_date) %>% substr(1, 4)
qt <- lubridate::quarter(end_date, fiscal_start = 4)     


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
  
  read_rds(glue("{cl_out}/lookups/Unicode/",
                "Deprivation/postcode_2020_1_simd2020v2.rds")) %>%
    
    clean_names() %>%
    
    select(pc7, simd = simd2020v2_sc_quintile) %>%
    
    mutate(
      simd = case_when(
        simd == 1 ~ "1 - Most Deprived",
        simd == 5 ~ "5 - Least Deprived",
        TRUE ~ as.character(simd)
      )
    )
  
}


### 8 - Create folder structure ----

# Create data folder for FY and Qtr
use_directory(
  glue("data/{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
)

# Create output folders
use_directory("management-report/output")
use_directory("publication/output")
use_directory("publication/markdown/figures")


### END OF SCRIPT ###