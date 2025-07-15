#########################################################################
# Name of file - 00_setup-pub-environment.R
# Data release - Dementia PDS Analytical Outputs
# Original Authors - Alice Byers
# Original Date - March 2021
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Sets up environment required for running annual 
#               publication. This is the only file 
#               to be updated every time the process is run.
#########################################################################

### run MI set up ----
source(here::here("code", "00_setup-environment.R"))

### 0 - Manual Variable(s) - TO UPDATE ----

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(31032025) 

# UPDATE - Date of publication (ddmmyyyy)
pub_date <- lubridate::dmy(29072025)

# UPDATE - Date of last publication (ddmmyyyy)
last_pub_date <- lubridate::dmy(28052024)


### 1 - Load packages ----

library(english)       # For converting number to text
#library(captioner)
library(readxl)        # For reading xlsx workbooks

# ### 2 - Define file paths dependent on whether running on server or desktop ----
# 
# stats <- case_when(
#   sessionInfo()$platform == "x86_64-pc-linux-gnu" ~ "/conf",
#   TRUE ~ "//stats"
# )
# 
# cl_out <- case_when(
#   sessionInfo()$platform == "x86_64-pc-linux-gnu" ~ 
#     "/conf/linkage/output",
#   TRUE ~ "//stats/cl-out"
# )


### 3 - Extract dates ----

# Latest FY and Quarter
fy <- extract_fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)    

# FYs included in pub
fy_in_pub <-  
  seq.Date(dmy(01042016), 
           dmy(glue("0104{year(pub_date) - 3}")), 
           "years") %>%
  extract_fin_year()

# Publication years
latest_fy  <- nth(fy_in_pub, -1)
revised_fy <- nth(fy_in_pub, -2)


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

# Load functions
source(here::here("functions/setup_directories.R"))


### END OF SCRIPT ###