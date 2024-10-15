################################################################################
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
#               to be updated everytime the process is run.
################################################################################


### 0 - Manual Variable(s) - TO UPDATE 

# UPDATE - TRUE/FALSE for defining a test file path for saving test copies of 
#           outputs. This is useful for when the DM give us a test run when boards
#           are still submitting the data.
#
#           test_report = TRUE - returns the test file path
#           test_report = FALSE - returns the finalised report for distribution
test_output <- TRUE

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(30062024)

# UPDATE - Most recent Date of publication (ddmmyyyy)
# Need this for set up of some folder structure
pub_date <- lubridate::dmy(26032024)

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
#library(flexdashboard) # For creating markdown outputs
library(purrr)         # For functional programming
library(forcats)       # For factor manipulation
library(knitr)         # For creating kable tables
library(kableExtra)    # For customising kable tables
library(phsmethods)    # For formatting postcode
library(openxlsx)      # For working with Excel files
library(flextable)     # For formatted tables in publication output
library(usethis)       # For creating folder structure
#library(rmarkdown)     # For render function
library(officer)       # For adding cover page and toc to report
library(gluedown)      # For formatting character vectors in markdown
library(fs)            # For setting up directories 


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


### 3 - SIMD Lookup ----

simd <- function(){
  read_rds(glue("{cl_out}/lookups/Unicode/",
                "Deprivation/postcode_2024_1_simd2020v2.rds")) %>%
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


### 4 - Derive dates ----

# Latest FY and Quarter
fy <- extract_fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)

# First date in reporting period 
start_date <- dmy(01042016)
  
### 5 - Set output/knitr options ----

# Disable scientific notation
options(scipen = 999)

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=",")
})


### 6 - Define exempt termination reason codes ----

exempt_reasons <- c("03", "04", "05", "06")


### 7 - Create folder structure ----

# Load functions
source(here::here("functions/setup_directories.R"))


#Define years in which data has been made final
finalised_years <- 
  list.files(get_final_data_dir()) %>% 
  str_sub(1, 7) %>%
  str_replace("-", "/")


### END OF SCRIPT ###