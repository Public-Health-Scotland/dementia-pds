#########################################################################
# Name of file - 03_knit-markdown.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - December 2020
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Knit markdown documents to create summary and report.
#########################################################################


### 1 - Load environment file and load functions then knit SUMMARY ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

render_check(
  input = here("publication", "markdown", "summary.Rmd"),
  output_file = get_pub_output_path(output_name = "pub_summary", test_output = test_output)
)

### 2 - load environment file and load functions then knit REPORT ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

render_check(
  input = here("publication", "markdown", "report.Rmd"),
  output_file = get_pub_output_path(output_name = "pub_report", test_output = test_output)
)

### END OF SCRIPT ###
