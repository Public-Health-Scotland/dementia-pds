#########################################################################
# Name of file - 05_create-pub-docs.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - March 2020
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Render markdown to produce summary and report word docs.
#
#########################################################################


### 1 - Load environment file and functions ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Render markdown summary ----

rmarkdown::render(
  input = here("publication", "markdown", 
               "summary.Rmd"),
  output_file = here("publication", "output", 
                     glue("{pub_date}_summary.docx"))
)


### END OF SCRIPT ###