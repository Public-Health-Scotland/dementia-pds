#########################################################################
# Name of file - 1_data_preparation.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Read in data file from data management and prepare for
# adding measures and producing outputs.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Load environment file ----

source("code/0_setup_environment.R")


### 2 - Read in data ----

pds <- read_csv(glue("{filepath}dementia/03-Outputs/zNational/",
                     "National_DementiaPDS_{fy}_Q{qt}.csv"),
                col_types = cols(.default = "c")) %>%

  clean_names() %>%
  
  # Convert dates from character to date format
  mutate_at(vars(contains("date")), funs(lubridate::dmy(.))) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = if_else(nchar(chi_number) == 9,
                              paste0("0", chi_number),
                              chi_number))
  
       


