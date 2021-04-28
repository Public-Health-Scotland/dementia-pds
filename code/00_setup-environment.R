#########################################################################
# Name of file - 00_setup-environment.R
# Data release - Dementia PDS Analytical Outputs
# Original Authors - Alice Byers
# Original Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Sets up environment required for running quarterly 
#               management reports and publication. This is the only file 
#               to be updated everytime the process is run.
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
library(forcats)       # For factor manipulation
library(knitr)         # For creating kable tables
library(kableExtra)    # For customising kable tables
library(phsmethods)    # For formatting postcode
library(openxlsx)      # For working with Excel files
library(flextable)     # For formatted tables in publication output
library(usethis)       # For creating folder structure
library(rmarkdown)     # For render function
library(officer)       # For adding cover page and toc to report


### 2 - Dates - UPDATE THIS SECTION ----

# Last day in reporting period
end_date   <- dmy(31122020)


### 3 - Define file paths dependent on whether running on server or desktop ----

stats <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ "/conf",
  TRUE ~ "//stats"
)

cl_out <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ 
    "/conf/linkage/output",
  TRUE ~ "//stats/cl-out"
)


### 4 - Derive dates ----

# Latest FY and Quarter
fy <- fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)     


### 5 - Disable scientific notation ----

options(scipen = 999)


### 6 - Set knitr options ----

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=",")
})


### 7 - Define exempt termination reason codes ----

exempt_reasons <- c("03", "04", "05", "06")


### 8 - Create folder structure ----

# Create data folder for FY and Qtr
use_directory(
  glue("data/{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
)

# Create output folders
use_directory("management-report/output")


### END OF SCRIPT ###