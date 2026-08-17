################################################################################.
# Name of file - 00_setup-environment.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - 
#   Sets up environment required for running quarterly management reports. 
#   This is the only file to be updated every time the process is run.
################################################################################.

################################################################################.
### 0 - Manual Variable(s) - TO UPDATE 
################################################################################.

# UPDATE - Financial Year (YYYY) and Quarter (Q) of the submission data
# Each submission contains data for the previous 3 full financial years and the current financial year up to the current quarter.
# Sep-MI-release: Q1 data (1 Apr - 30 June)
# Dec-MI-release: Q2 data (1 Jul - 30 Sept)
# Mar-MI-release: Q3 data (1 Oct - 31 Dec)
# Jun-MI-release: Q4 data (1 Jan - 31 March)
fy <- 2025
qt <- 3

# UPDATE - Used to define a test file path for saving test outputs (TRUE/FALSE)
# TRUE:  Use this when the Data Management Team provide a test version of the 
#        collated file and boards are still submitting or correcting data.
# FALSE: Use this when the Data Management Team provide a finalised version of  
#        the collated file and no more changes will be made.
test_output <- F

################################################################################.
### 1 - Load packages ----
################################################################################.

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
library(phsopendata)   # For reading European Standard Population
library(openxlsx)      # For working with Excel files
library(flextable)     # For formatted tables in publication output
library(usethis)       # For creating folder structure
library(rmarkdown)     # For render function
library(officer)       # For adding cover page and toc to report
library(gluedown)      # For formatting character vectors in markdown
library(fs)            # For setting up directories 

################################################################################.
### 2 - Load functions ----
################################################################################.

# Load file path functions
source(here::here("functions/setup_directories.R"))

# Use write file function for writing files to disk and setting correct permissions
source(here::here("functions/write_file.R"))

# Use render_check function for rendering rmarkdown files
source(here::here("functions/render_check.R"))

################################################################################.
### 3 - Derive dates ----
################################################################################.

# First date in the reporting period (1 Apr 2016)
start_date <- dmy(01042016)

# End date for the current submission
end_date_month <- case_when(
  qt == 1 ~ "3006",
  qt == 2 ~ "3009",
  qt == 3 ~ "3112",
  qt == 4 ~ "3103"
)
end_date_year <- ifelse(qt == 4, fy + 1, fy)
end_date <- lubridate::dmy(paste0(end_date_month, end_date_year))

# Helper function to return current, provisional and revised years
get_fy <- function(date, modifier = 0){
  fy <- phsmethods::extract_fin_year(date)
  fy <- sprintf(
    "%d/%02d",
    as.numeric(substr(fy, 1, 4)) - modifier,
    as.numeric(substr(fy, 6, 7)) - modifier
  )
  return(fy)
}

# Current, provisional and revised years
current_fy <- get_fy(end_date)
extra_referrals_year <- get_fy(end_date, 1)
provisional_year <- get_fy(end_date, 2)
revised_year <- get_fy(end_date, 3)

# Years that have been finalised
fy_range <- function(start_year, end_year) {
  sprintf(
    "%d/%02d",
    start_year:end_year,
    (start_year:end_year + 1) %% 100)
}

finalised_years <- fy_range(
  as.numeric(substr(get_fy(start_date), 1, 4)),
  as.numeric(substr(get_fy(end_date, 4), 1, 4))
)

if (qt == 4){
  finalised_years <- c(finalised_years, revised_year)
}

# Convert fy to a string
fy <- as.character(fy)

# All years
all_years <- fy_range(
  as.numeric(substr(get_fy(start_date), 1, 4)),
  as.numeric(substr(get_fy(end_date), 1, 4))
)

# Full years
if (qt == 4) {
  full_years <- all_years
} else {
  full_years <- all_years[-length(all_years)]
}

# Last full year
last_full_year <- full_years[length(full_years)]

################################################################################.
### 4 - Define exempt termination reason codes ----
################################################################################.

exempt_reasons <- c("03", "04", "05", "06")

################################################################################.
### 6 - Set output/knitr options for Markdown ----
################################################################################.

# Disable scientific notation
options(scipen = 999)

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x){
  if(!is.character(x)){prettyNum(x, big.mark=",")}else{x}
})

################################ END OF SCRIPT #################################.