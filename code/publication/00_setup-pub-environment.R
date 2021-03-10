#########################################################################
# Name of file - 00_setup-pub-environment.R
# Data release - Dementia PDS Analytical Outputs
# Original Authors - Alice Byers
# Original Date - March 2021
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Sets up environment required for running annual 
#               publication. This is the only file 
#               to be updated every time the process is run.
#########################################################################


### 1 - Load packages ----

library(dplyr)         # For data manipulation in the "tidy" way
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidyr)         # For data manipulation in the "tidy" way
library(stringr)       # For string manipulation and matching
library(here)          # For the here() function
library(glue)          # For working with strings
library(ggplot2)       # For plotting
library(plotly)        # For interactive plots
library(purrr)         # For functional programming
library(forcats)       # For factor manipulation
library(knitr)         # For creating kable tables
library(phsmethods)    # For formatting postcode
library(openxlsx)      # For working with Excel files
library(flextable)     # For formatted tables in publication output
library(usethis)       # For creating folder structure
library(rmarkdown)     # For render function
library(officer)       # For adding cover page and toc to report


### 2 - Dates - UPDATE THIS SECTION ----

# Last day in reporting period
end_date   <- dmy(31122020)

# Date of publication
# Only used when running publication, can comment out otherwise
pub_date <- dmy(30032021)

# FYs included in pub
if(exists("pub_date")){
  fy_in_pub <-  
    seq.Date(dmy(01042016), 
             dmy(glue("0104{year(pub_date) - 3}")), 
             "years") %>%
    fin_year()
}


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


### 4 - Extract dates ----

# Start date of time period to be submitted
# Note that previous years are reported but no longer submitted
start_date <- ymd(
  paste0(if_else(month(end_date) >= 4,
                 year(end_date) - 3,
                 year(end_date) - 4),
         "0401")
)

# Latest FY and Quarter
fy <- fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)     


### 5 - Disable scientific notation ----

options(scipen = 999)


### 6 - Set knitr options ----

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x){
  if(!is.character(x)){prettyNum(x, big.mark=",")}else{x}
})


### END OF SCRIPT ###