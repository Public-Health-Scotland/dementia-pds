#########################################################################
# Name of file - 01_knit-markdown.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - December 2020
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Knit markdown documents to create summary and report.
#########################################################################


### 0 - Load environment file and functions ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))

source(here("functions", "create_report.R"))


### 1 - Knit summary ----

render(
  input = here("publication", "markdown", "summary.Rmd"),
  output_file = here("publication", "output", pub_date,
                     paste0(pub_date, "_summary.docx"))
)


### 2 - Knit report ----

create_report(pub_date, max(fy_in_pub))


### END OF SCRIPT ###