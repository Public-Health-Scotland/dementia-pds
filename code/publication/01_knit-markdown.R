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


### 4 - Knit report ----

params <- list(
  rmd_filename    = here("publication", "markdown", "report.Rmd"),
  cover_filename  = here("publication", "markdown", "templates", 
                         "report-cover-page.docx"),
  title           = "Dementia Post-Diagnostic Support",
  subtitle        = paste("Local Delivery Plan Standard - Figures for",
                          glue_collapse(fy_in_pub, sep = ", ", last = " and ")),
  date1           = pub_date %>% format("%e %B %Y") %>% str_trim(),
  date2           = pub_date %>% format("%d/%m/%Y"),
  filename_out    = here("publication", "output", pub_date, 
                         paste0(pub_date, "_report.docx"))
)

source(here("code", "publication", "compile.R"))


### END OF SCRIPT ###