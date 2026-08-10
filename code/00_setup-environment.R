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

# UPDATE - Used to define a test file path for saving test outputs (TRUE/FALSE)
# TRUE:  Use this when the Data Management Team provide a test version of the 
#        collated file and boards are still submitting or correcting data.
# FALSE: Use this when the Data Management Team provide a finalised version of  
#        the collated file and no more changes will be made.
test_output <- TRUE

# UPDATE - Last day in the current reporting period (ddmmyyyy)
# Sep-MI-release (Q1 data): 300620XX
# Dec-MI-release (Q2 data): 300920XX
# Mar-MI-release (Q3 data): 311220XX
# Jun-MI-release (Q4 data): 310320XX
end_date <- lubridate::dmy(31032026)

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
### 3 - Derive dates ----
################################################################################.

# FY and Quarter for current reporting period
fy <- phsmethods::extract_fin_year(end_date) %>% substr(1, 4)
qt <- lubridate::quarter(end_date, fiscal_start = 4)

# FY and Quarter for previous reporting period
previous_end_date <- ceiling_date(end_date %m-% months(3), "month") - days(1)
previous_fy <- phsmethods::extract_fin_year(previous_end_date) %>% substr(1, 4)
previous_qt <- lubridate::quarter(previous_end_date, fiscal_start = 4)

# First date in reporting period (ddmmyyyy)
start_date <- dmy(01042016)

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

if (qt == 4){
  finalised_years <- c(finalised_years, revised_year)
}

################################################################################.
### 4 - Define exempt termination reason codes ----
################################################################################.

exempt_reasons <- c("03", "04", "05", "06")

################################################################################.
### 5 - SIMD lookup ----
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
### 6 - Define file paths dependent on whether running on server or desktop ----
################################################################################.

stats <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu" ~ "/conf",
  TRUE ~ "//stats"
)

cl_out <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu" ~ 
    "/conf/linkage/output",
  TRUE ~ "//stats/cl-out"
)

################################################################################.
### 7 - Set output/knitr options for Markdown ----
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