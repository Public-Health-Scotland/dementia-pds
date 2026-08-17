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
# Description - Sets up environment required for running quarterly 
#               management reports. This is the only file 
#               to be updated every time the process is run.
################################################################################.

################################################################################.
### 0 - Manual Variable(s) - TO UPDATE 
################################################################################.

# UPDATE - TRUE/FALSE for defining a test file path for saving test copies of 
#           outputs. This is useful for when the DM give us a test run when boards
#           are still submitting the data or when making changes to the code.
#
#           test_output = TRUE - returns the test file path for data and report
#           test_output = FALSE - returns the finalised data and report for distribution
test_output <- FALSE

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(31122025)
previous_end_date <- lubridate::dmy(30092025)

# UPDATE - Most recent Date of publication (ddmmyyyy)
# Need this for set up of some folder structure
pub_date <- lubridate::dmy(16122025)

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
### 3 - Define file paths dependent on whether running on server or desktop ----
################################################################################.

# Use render_check function for rendering rmarkdown files
source(here::here("functions/render_check.R"))

################################################################################.
### 3 - Derive dates ----
################################################################################.

# First date in the reporting period (1 Apr 2016)
start_date <- dmy(01042016)

# End date for the current submission
end_date <- case_when(
  qt == 1 ~ "3006",
  qt == 2 ~ "3009",
  qt == 3 ~ "3112",
  qt == 4 ~ "3103"
)
end_date <- lubridate::dmy(paste0(end_date, fy))

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
current_year <- get_fy(end_date)
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

################################################################################.
### 4 - SIMD Lookup ----
################################################################################.

simd <- function(){
  simd <- read_rds(get_simd_path()) %>% 
    clean_names() %>%
    select(pc7, simd = simd2020v2_sc_quintile) %>%
    mutate(
      simd = case_when(
        simd == 1 ~ "1 - Most Deprived",
        simd == 5 ~ "5 - Least Deprived",
        TRUE ~ as.character(simd)
      )
    )
  
  return(simd)
}

################################################################################.
### 5 - Derive dates ----
################################################################################.

# Latest FY and Quarter
fy <- extract_fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)

cl_out <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu" ~ 
    "/conf/linkage/output",
  TRUE ~ "//stats/cl-out"
)

# First date in reporting period 
start_date <- dmy(01042016)
  
# Define years in which data has been made final
finalised_years <- 
  list.files(get_finalised_data_dir()) %>% 
  str_sub(1, 7) %>%
  str_replace("-", "/")

finalised_years_referrals <- finalised_years [-c((length(finalised_years)-1), length(finalised_years))]
finalised_years_demographics <- finalised_years [-c((length(finalised_years)-2), (length(finalised_years)-1), length(finalised_years))]

################################################################################.
### 6 - Set output/knitr options ----
################################################################################.

# Disable scientific notation
options(scipen = 999)

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x) {
  if(!is.character(x)) {prettyNum(x, big.mark=",")} else {x}
})

################################################################################.
### 7 - Define exempt termination reason codes ----
################################################################################.

exempt_reasons <- c("03", "04", "05", "06")

################################ END OF SCRIPT #################################.