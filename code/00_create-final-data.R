#########################################################################
# Name of file - 00_create-final-data.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - September 2020
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Create finalised data file for years no longer submitted.
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))

fy_final <- "2017-18"

start_final <- dmy(paste0("0104", str_sub(fy_final, 1, 4)))
end_final   <- dmy(paste0("310320", str_sub(fy_final, 6, 7)))


### 2 - Read in Q3 collated file from previous FY ----

pds <- 
  
  read_csv(glue("{stats}/dementia/03-Outputs/National/",
                "{fy}-Q{qt}_national.csv"),
           col_types = cols(.default = "c")) %>%
  
  clean_names() %>%
  
  # Convert dates from character to date format
  mutate(across(contains("date"), ymd)) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = chi_pad(chi_number)) %>%
  
  # Replace word 'and' with ampersand
  mutate(health_board = str_replace(health_board, " and ", " & ")) %>%
  
  # Rename 'Gender' to 'Sex'
  rename_with(~"sex", matches("gender"))


### 3 - Extract records with diagnosis date before start date ----

pds %<>%
  filter(between(dementia_diagnosis_confirmed_date, start_final, end_final))


### 4 - Save final file ----

write_rds(
  pds,
  here("data", "final",
       glue("{fy_final}_final-data.rds")),
  compress = "gz"
)


### END OF SCRIPT ###