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
# render_check firsts checks if a file already exists before running rmarkdown::render
# if a file already exists, permission must be given via the console to overwrite the file

render_check(
  input = here("management-report", "markdown", 
               "management-report.Rmd"),
  output_file = get_mi_output_path(test_output = test_output))


### END OF SCRIPT ###