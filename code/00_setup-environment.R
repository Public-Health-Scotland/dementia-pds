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

output_path <- "//conf/dementia/A&I/Analysts/Lucy/Age_Standardisation/"

# UPDATE - Used to define a test file path for saving test outputs (TRUE/FALSE)
# TRUE:  Use this when the Data Management Team provide a test version of the 
#        collated file and boards are still submitting or correcting data.
# FALSE: Use this when the Data Management Team provide a finalised version of  
#        the collated file and no more changes will be made.
test_output <- TRUE

# UPDATE - Last day in the current reporting period (ddmmyyyy)
# Mar-MI-release: 311220XX
# Jun-MI-release: 310320XX
# Sep-MI-release: 300620XX
# Dec-MI-release: 300920XX
end_date <- lubridate::dmy(31032026)

# UPDATE - Last day in the previous reporting period (ddmmyyyy)
# Mar-MI-release: 310920XX
# Jun-MI-release: 311220XX
# Sep-MI-release: 310320XX
# Dec-MI-release: 310620XX
previous_end_date <- lubridate::dmy(31122025)

# UPDATE - Most recent Date of publication (ddmmyyyy)
# Found in: 00_setup-pub-environment.R
pub_date <- lubridate::dmy(16122025)

################################################################################.
### 1 - Load functions and packages ----
################################################################################.

# Load functions
source(here::here("functions/setup_directories.R"))

# Load packages
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
### 2 - Define file paths dependent on whether running on server or desktop ----
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
### 3 - SIMD lookup ----
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
### 4 - Derive dates ----
################################################################################.

# Latest FY and Quarter
fy <- phsmethods::extract_fin_year(end_date) %>% substr(1, 4)
qt <- lubridate::quarter(end_date, fiscal_start = 4)

# Previous FY and Quarter
previous_fy <- phsmethods::extract_fin_year(previous_end_date) %>% substr(1, 4)
previous_qt <- lubridate::quarter(previous_end_date, fiscal_start = 4)

# First date in reporting period (ddmmyyyy)
start_date <- dmy(01042016)
  
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

################################################################################.
### 6 - Define exempt termination reason codes ----
################################################################################.

exempt_reasons <- c("03", "04", "05", "06")

################################################################################.
### 7 - Define finalised years ----
################################################################################.

# Define years in which data has been made final
finalised_years <- 
  list.files(get_final_data_dir()) %>% 
  str_sub(1, 7) %>%
  str_replace("-", "/")

################################ END OF SCRIPT #################################.