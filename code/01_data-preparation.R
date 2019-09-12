#########################################################################
# Name of file - 01_data-preparation.R
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

source(here::here("code", "00_setup-environment.R"))
source(here::here("functions", "financial_year.R"))

# Create data folder if this doesn't already exist
if(!("data" %in% fs::dir_ls())){fs::dir_create("data")}


### 2 - Read and clean collated file ----

pds <- 
  
  read_csv(glue("{filepath}dementia/03-Outputs/zNational/",
                     "National_DementiaPDS_{fy}_Q{qt}.csv"),
                col_types = cols(.default = "c")) %>%

  clean_names() %>%
  
  # Convert dates from character to date format
  mutate_at(vars(contains("date")), ~ lubridate::ymd(.)) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = if_else(nchar(chi_number) == 9,
                              paste0("0", chi_number),
                              chi_number)) %>%
  
  # Replace word 'and' with ampersand
  mutate(health_board = str_replace(health_board, " and ", " & "))


### 3 - Save out error summary

pds %>%
  mutate(fy = financial_year(dementia_diagnosis_confirmed_date)) %>%
  group_by(fy, health_board, ijb) %>%
  summarise(errors  = sum(!is.na(record_has_error)),
            records = n()) %>%
  arrange(fy, health_board, ijb) %>%
  write_rds(here("data", glue("{fy}-{qt}_error-summary.csv")))


### 4 - Recode Lanarkshire IJB records ----

pds %<>%
  mutate(health_board = 
            case_when(
               str_detect(ijb, "S37000035|S37000028") ~ "L NHS Lanarkshire",
                          TRUE                        ~ health_board
            ))


### 5 - Recode errors ----
# TO DO - review once number of errors known

pds %<>%
  
  # Remove records with missing CHI and missing diagnosis date
  filter(!is.na(dementia_diagnosis_confirmed_date) & !is.na(chi_number)) %>%

  # Select records within reporting period only
  filter(dementia_diagnosis_confirmed_date %within% 
           interval(start_date, end_date)) %>%
  
  # Recode missing PDS Status
  mutate(pds_status = 
           case_when(
             str_detect(pds_status, "02") ~ "02 Inactive",
             is.na(pds_status)            ~ "01 Active",
             TRUE                         ~ pds_status
           )) %>%

  # Recode NAs to 99 Not Known (for variables where this is an option)
  mutate_at(vars("gender", "ethnic_group", "additional_disability",
                 "living_alone", "accommodation_type", "pds_referral_source",
                 "carers_support"), 
            ~ replace_na(., "99 Not Known"))


### 6 - Remove duplicates ----
# TO DO - review once number of duplicates is known
       
pds %<>%
  
  distinct(chi_number, .keep_all = TRUE)


### 7 - Save data ---

write_csv(pds, here("data", glue("{fy}-{qt}_clean-data.csv")))


### END OF SCRIPT ###