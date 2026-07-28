################################################################################.
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
################################################################################.

################################################################################.
### 0 - Load environment file
################################################################################.

source(here::here("code", "publication", "00_setup-pub-environment.R"))

################################################################################.
### 1 - Knit SUMMARY ----
################################################################################.

render_check(
  input = here("publication", "markdown", "latest_summary.Rmd"),
  output_file = get_pub_output_path(
    output_name = "pub_summary", 
    pub_date = pub_date,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE
  )
)

################################################################################.
### 2 - Knit REPORT ----
################################################################################.

render_check(
  input = here("publication", "markdown", "latest_report.Rmd"),
  output_file = get_pub_output_path(
    output_name = "pub_report", 
    pub_date = pub_date,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE
  )
)

################################ END OF SCRIPT #################################.