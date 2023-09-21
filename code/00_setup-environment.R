################################################################################
# Name of file - 00_setup-environment.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Sets up environment required for running quarterly 
#               management reports. This is the only file 
#               to be updated everytime the process is run.
################################################################################


### 0 - Manual Variable(s) - TO UPDATE 

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(30062023)

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
                "Deprivation/postcode_2023_1_simd2020v2.rds")) %>%
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

mi_data_path <- function(type = c("error_data", 
                                  "dupe_data", 
                                  "clean_data", 
                                  "ldp_data", 
                                  "final_data"
                                  ),
                         ext = c("rds", 
                                 "csv")
                         ) {
  year_dir <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
  
  mi_dir <- dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", {year_dir}))
  
  file_name <- file_name <- dplyr::case_match(
    type,
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary.{ext}"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes.{ext}"), 
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data{ext}"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp.{ext}"), 
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data.{ext}")
    )
    
  mi_path <- stringr::str_glue("{mi_dir}/{file_name}")

  return(mi_path)
}



# Create data folder for FY and Qtr
use_directory(
  glue("data/{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
)

# Create output folder
use_directory(glue("management-report/output/",
                   "{fy}-{substr(as.numeric(fy)+1, 3, 4)}"))


### END OF SCRIPT ###