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
# TO DO - Review whether we want to roll forward or back

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


### 4 - Add LDP Standard Classification ----

pds %<>%
  
  mutate(ldp = case_when(
    
    ## COMPLETE ##
    
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    diag_12 > date_of_initial_first_contact & 
      is.na(termination_or_transition_date) & pds_12 < end_date
    ~ "complete",
    
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    diag_12 > date_of_initial_first_contact &
      pds_11 <= termination_or_transition_date
    ~ "complete",
    
    ## EXEMPT ##
    
    # Exempt termination reason; SU died/moved to other HB/refused/can't engage
    substr(termination_or_transition_reason, 1, 2) %in% 
      c("03", "04", "05", "06")
    ~ "exempt",
    
    ## ONGOING ##
    
    # Less than 12m since diagnosis and PDS not started
    diag_12 > end_date & 
      is.na(date_of_initial_first_contact) & is.na(termination_or_transition_reason)
    ~ "ongoing",
    
    # PDS started within 12m of diagnosis but not yet ended
    diag_12 > date_of_initial_first_contact &
      pds_12 > end_date &
      is.na(termination_or_transition_date)
    ~ "ongoing",
    
    ## FAIL ##
    
    # More than 12m since diagnosis and PDS not started
    diag_12 <= end_date &
      is.na(termination_or_transition_date)
    ~ "fail",
    
    # PDS started more than 12m after diagnosis
    diag_12 <= date_of_initial_first_contact
    ~ "fail",
    
    # PDS terminated before 11 months from start date
    pds_11 > termination_or_transition_date
    ~ "fail"
    
  ))


### 5 - Add Old Methodology LDP Classification ----

pds %<>%
  
  mutate(ldp_old = case_when())


### * - Add FY and months labels ----

pds %<>%
  
  mutate(fy    = finyear(dementia_diagnosis_confirmed_date),
         month = month(dementia_diagnosis_confirmed_date))
