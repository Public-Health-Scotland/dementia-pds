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


### 0 - Manual Variable(s) - TO UPDATE ----

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(31122022) 

# UPDATE - Date of publication (ddmmyyyy)
pub_date <- lubridate::dmy(28032023)

# UPDATE - Date of last publication (ddmmyyyy)
last_pub_date <- lubridate::dmy(29032022)


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
library(english)       # For converting number to text
library(captioner)


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

# Latest FY and Quarter
fy <- fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)    

# FYs included in pub
fy_in_pub <-  
  seq.Date(dmy(01042016), 
           dmy(glue("0104{year(pub_date) - 3}")), 
           "years") %>%
  fin_year()


### 4 - Disable scientific notation ----

options(scipen = 999)


### 5 - Set knitr options ----

# Allow duplicate labels
options(knitr.duplicate.label = "allow")

# Knitr hook to add thousands separator
knit_hooks$set(inline = function(x){
  if(!is.character(x)){prettyNum(x, big.mark=",")}else{x}
})


### 6 - Create folder for publication output ----

use_directory(glue("publication/output/{pub_date}"))


### END OF SCRIPT ###