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


### 1 - Load environment file and functions ----

source(here("code", "0_setup_environment.R"))


### 2 - Read in data ----

pds <- 
  
  read_csv(glue("{filepath}dementia/03-Outputs/zNational/",
                     "National_DementiaPDS_{fy}_Q{qt}.csv"),
                col_types = cols(.default = "c")) %>%

  clean_names() %>%
  
  # Convert dates from character to date format
  mutate_at(vars(contains("date")), funs(lubridate::dmy(.))) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = if_else(nchar(chi_number) == 9,
                              paste0("0", chi_number),
                              chi_number))


### 3 - Recode Lanarkshire IJB records ----
# TO DO - check if this recoding is built into DM process

pds %<>%
  
  mutate(health_board = 
           case_when(
             str_detect(ijb, "S37000035|S37000028") ~ "L NHS Lanarkshire",
             TRUE                                   ~ health_board
           ))


### 4 - Recode errors ----
# TO DO - review once number of errors known

pds %<>%
  
  # Recode missing PDS Status
  mutate(pds_status = 
           case_when(
             str_detect(pds_status, "02") ~ "02 Inactive",
             is.na(pds_status)            ~ "01 Active",
             TRUE                         ~ pds_status
           )) %>%

  # Remove missing CHI and missing diagnosis date
  filter(!is.na(dementia_diagnosis_confirmed_date) & !is.na(chi_number)) %>%


  # Select records in reporting period only
  filter(dementia_diagnosis_confirmed_date %within% 
           interval(start_date, end_date))


### 5 - Remove duplicates ----
# TO DO - review once number of duplicates is known
       
pds %<>%
  
  distinct(chi_number, .keep_all = TRUE)


### 6 - Save data ---

write_csv(pds, here("data", glue("{fy}Q{qt}_clean_data.csv")))


### END OF SCRIPT ###