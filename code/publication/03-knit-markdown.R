#########################################################################
# Name of file - 03_knit-markdown.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - December 2020
# Updated - Jennifer Thom, November 2023 
#
# Written/run on - RStudio Server.
# Version of R - 4.1.2
# Description - Knit markdown documents to create summary and report.
#########################################################################


### 1 - Load environment file and load functions then knit SUMMARY ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

render(
  input = here("publication", "markdown", "summary.Rmd"),
  output_file = output_path(
    directory = "publication",
    output_name = "pub_summary"
  )
)

### 2 - load environment file and load functions then knit REPORT ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

rmarkdown::render(
  input = here("publication", "markdown", "report.Rmd"),
  output_dir = output_path(
    directory = "publication",
    output_name = "pub_report"
  )
)

### END OF SCRIPT ###
