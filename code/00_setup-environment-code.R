################################################################################.
# Name of file - 00_setup-environment.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
# Updated by Jennifer Thom - Nov 2023
# Updated by Lucy Binsted - Oct 2026

# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - 
#   Sets up environment required for running quarterly management reports. 
################################################################################.

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
library(english)       # For converting number to text
library(readxl)        # For reading xlsx workbooks

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

# Helper function to return formatted financial year from date
get_fy <- function(date, modifier = 0){
  fy <- phsmethods::extract_fin_year(date)
  fy <- sprintf(
    "%d/%02d",
    as.numeric(substr(fy, 1, 4)) - modifier,
    as.numeric(substr(fy, 6, 7)) - modifier
  )
  return(fy)
}

# Helper function to return list of financial years
fy_range <- function(start_year, end_year) {
  sprintf(
    "%d/%02d",
    start_year:end_year,
    (start_year:end_year + 1) %% 100)
}

# First date in the reporting period (1 Apr 2016)
start_date <- dmy(01042016)

# End date for the current submission
end_date <- lubridate::dmy(
  paste0(
    case_when(
      qt == 1 ~ "3006",
      qt == 2 ~ "3009",
      qt == 3 ~ "3112",
      qt == 4 ~ "3103"
    ), 
    ifelse(qt == 4, fy + 1, fy)
  )
)

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

# Finalised years
if (qt == 4){
  finalised_years <- fy_range(
    as.numeric(substr(get_fy(start_date), 1, 4)),
    as.numeric(substr(get_fy(end_date, 3), 1, 4))
  )
} else {
  finalised_years <- fy_range(
    as.numeric(substr(get_fy(start_date), 1, 4)),
    as.numeric(substr(get_fy(end_date, 4), 1, 4))
  )
}

# Specific years (formatted as yyyy/yy)
current_fy <- tail(all_years, 1)
last_full_year <- tail(full_years, 1)
extra_referrals_year <- setdiff(all_years, finalised_years)[2]
provisional_year <- setdiff(all_years, finalised_years)[1]
revised_year <- tail(finalised_years, 1)

# Publication years
if(exists("pub_date")){
  fy_in_pub <- seq.Date(
    start_date, 
    dmy(glue("0104{year(pub_date) - 3}")), 
    "years") %>%
    extract_fin_year()
  
  latest_fy  <- tail(fy_in_pub, 1)
  revised_fy <- tail(fy_in_pub, 2)
  test_output <- FALSE
}

################################################################################.
### 4 - Define exempt termination reason codes ----
################################################################################.

exempt_reasons <- c("03", "04", "05", "06")

################################################################################.
### 5 - Set output/knitr options for Markdown ----
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