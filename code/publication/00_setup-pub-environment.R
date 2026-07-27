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

### Run MI set up ----
source(here::here("code", "00_setup-environment.R"))

### 0 - Manual Variable(s) - TO UPDATE ----

# UPDATE - Last day in reporting period (ddmmyyyy)
end_date <- lubridate::dmy(31122025) 

# UPDATE - Date of publication (ddmmyyyy)
pub_date <- lubridate::dmy(21072026)

# UPDATE - Date of last publication (ddmmyyyy)
last_pub_date <- lubridate::dmy(16122025)

test_output <- FALSE

### 1 - Load additional packages ----

library(english)       # For converting number to text
library(readxl)        # For reading xlsx workbooks

### 2 - Extract publication dates ----

# Latest FY and Quarter
fy <- extract_fin_year(end_date) %>% substr(1, 4)
qt <- quarter(end_date, fiscal_start = 4)    

# FYs included in publication
fy_in_pub <- seq.Date(
  dmy(01042016), 
  dmy(glue("0104{year(pub_date) - 3}")), 
  "years") %>%
  extract_fin_year()

# Publication years
latest_fy  <- nth(fy_in_pub, -1)
revised_fy <- nth(fy_in_pub, -2)

### END OF SCRIPT ###