#########################################################################
# Name of file - 03_create-management-report.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - September 2019
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Render markdown to produce html report.
#########################################################################


### 1 - Load environment file and functions ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Render markdown script ----

rmarkdown::render(
  input = here("management-report", "markdown", 
               "management-report.Rmd"),
  output_file = output_path(directory = "mi", 
                            output_name = "mi_report"))


### END OF SCRIPT ###