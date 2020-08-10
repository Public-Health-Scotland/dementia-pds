#########################################################################
# Name of file - 03_create-management-report.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - September 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Render markdown to produce html report.
#########################################################################


### 1 - Load environment file and functions ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Render markdown script ----

rmarkdown::render(
  input = here("management-report", "markdown", 
               "management-report.Rmd"),
  output_file = here("management-report", "output", 
                     glue("{end_date}_management-report.html"))
)


### END OF SCRIPT ###