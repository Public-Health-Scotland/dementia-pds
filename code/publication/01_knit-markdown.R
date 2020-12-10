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


### 1 - Turn off kableExtra auto formatting ----

# Causes issues when knitting kable to word
options(kableExtra.auto_format = FALSE)


### 2 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 3 - Knit summary ----

render(
  input = here("publication", "markdown", "summary.Rmd"),
  output_file = here("publication", "output", pub_date,
                     paste0(pub_date, "_summary.docx"))
)


### END OF SCRIPT ###