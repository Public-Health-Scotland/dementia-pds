#########################################################################
# Name of file - 2_calculate_measures.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Add LDP Standard classification and other measures to be
# included in reports.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Load environment file and functions ----

source("code/0_setup_environment.R")
source("code/functions.R")


### 2 - Load data ----

pds <- read_csv(here("data", glue("{fy}Q{qt}_clean_data.csv")))


### 3 - Add key dates for calculations ----

pds %<>%
  
  mutate(
    
    # Date 12 months after diagnosis date
    diag_12       = add_with_rollback(dementia_diagnosis_confirmed_date, 
                                      months(12),
                                      roll_to_first = TRUE),
    
    # Date 11 months after date of first PDS contact     
    pds_11        = add_with_rollback(date_of_initial_first_contact, 
                                      months(11),
                                      roll_to_first = TRUE),
    
    # Date 12 months after date of first PDS contact
    pds_12        = add_with_rollback(date_of_initial_first_contact, 
                                      months(12),
                                      roll_to_first = TRUE),
    
    # Number of months between diagnosis and date of first PDS contact
    time_to_start = trunc(time_length(
                             interval(dementia_diagnosis_confirmed_date, 
                                      date_of_initial_first_contact), 
                          "months"))
    
  )


### * - Add FY and months labels

pds %<>%
  
  mutate(fy    = finyear(dementia_diagnosis_confirmed_date),
         month = month(dementia_diagnosis_confirmed_date))
