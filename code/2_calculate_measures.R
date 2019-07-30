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


### 1 - Load environment file ----

source("code/0_setup_environment.R")


### 2 - Add key dates for calculations ----

pds %<>%
  
  mutate(
    
    # Date 12 months after diagnosis date
    diag_12       = add_months(dementia_diagnosis_confirmed_date, 12),
    
    # Date 11 months after date of first PDS contact     
    pds_11        = add_months(date_of_initial_first_contact, 11),
    
    # Date 12 months after date of first PDS contact
    pds_12        = add_months(date_of_initial_first_contact, 12),
    
    # Number of months between diagnosis and date of first PDS contact
    time_to_start = trunc(time_length(
      interval(dementia_diagnosis_confirmed_date, 
               date_of_initial_first_contact), 
      "months"))
    
  )

