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


### 3 - Add FY and months labels ----

pds %<>%
  
  mutate(fy    = finyear(dementia_diagnosis_confirmed_date),
         month = month(dementia_diagnosis_confirmed_date))


### 4 - Add key dates for calculations ----
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


### 5 - Add LDP standard classification ----

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


### 6 - Add old LDP standard classification ----

pds %<>%
  
  ## FAIL ##
  
  mutate(ldp_old = case_when(
    
    # Wait time longer than 12 months
    time_to_start > 12 ~ "fail",
    
    # Still waiting after 12 months
    is.na(date_of_initial_first_contact) &
      !(substr(termination_or_transition_reason, 1, 2) %in% c("03", "04")) &
      #!is.na(remove_reason) &      # condition on remove reason not missing causing records 'still waiting' to be marked as complete
      substr(pds_status, 1, 2) != "02" &     
      diag_12 < end_date ~ "fail",
    
    # Removed from service before 12 months with no reason provided
    # TO DO: Impossible
    # (remove_date < pds_end & is.na(remove_date)) &
    #   is.na(remove_reason) ~ "fail",
    
    # Remove reason 12 months complete but removed before 12 months
    substr(termination_or_transition_reason, 1, 2) == "01" & termination_or_transition_date < pds_11 ~ "fail",
    
    # Removed for non-exempt reasons before 12 months
    (termination_or_transition_date < pds_11 | is.na(termination_or_transition_date)) &
      substr(termination_or_transition_reason, 1, 2) %in% c("02", "98", "99") ~ "fail"
    
  )) %>%
  
  ## EXEMPT ##
  
  mutate(ldp_old = case_when(
    is.na(ldp_old) & substr(termination_or_transition_reason, 1, 2) %in% c("03", "04") ~ "exempt",
    TRUE ~ ldp_old
  )) %>%
  
  ## ONGOING ##
  
  mutate(ldp_old = case_when(
    (is.na(ldp_old) & (pds_11 > end_date | is.na(date_of_initial_first_contact))) &
      (!is.na(date_of_initial_first_contact) | (dementia_diagnosis_confirmed_date + months(12)) >= end_date) ~ "ongoing",
    TRUE ~ ldp_old
  )) %>%

  ## COMPLETE ##
  
  mutate(ldp_old = case_when(is.na(ldp_old) ~ "complete",
                         TRUE ~ ldp_old))


### 7 - Add flag for waiting list ----
# check against ldp flags; ongoing, complete, exempt should not be included on waiting list
# track wait list for every month from april 2016 to latest reporting month
# can we use inactive to remove people from waiting list or dates more reliable?


### 8 - Add waiting time categories

pds %<>%
  
  mutate(wait_length = case_when(
    
    is.na(time_to_start)           ~ "Still Waiting",
    between(time_to_start, 0, 3)   ~ "3 Months or Less",
    between(time_to_start, 4, 6)   ~ "4-6 Months",    
    between(time_to_start, 7, 9)   ~ "7-9 Months",
    between(time_to_start, 10, 12) ~ "10-12 Months",
    time_to_start >= 13            ~ "Over 1 Year",
    
  ))


### 9 - Create final output file ----

pds %<>%
  group_by(health_board, ijb, fy, month, ldp, ldp_old, wait_length) %>%
  summarise(referrals = n())

write_csv(pds, here("data", glue("{fy}Q{qt}_final_data.csv")))


### END OF SCRIPT ###
