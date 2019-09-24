#########################################################################
# Name of file - 02_calculate-measures.R
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

source(here::here("code", "00_setup-environment.R"))
source(here::here("functions", "financial_year.R"))


### 2 - Load data ----

pds <- read_rds(here("data", glue("{fy}-{qt}_clean-data.rds")))


### 3 - Add FY and months labels ----

pds %<>%
  mutate(fy    = financial_year(dementia_diagnosis_confirmed_date),
         month = month(dementia_diagnosis_confirmed_date))


### 4 - Add key dates for calculations ----

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
                                      roll_to_first = TRUE)
    
  )


### 5 - Add LDP standard classification ----

pds %<>%
  
  mutate(ldp = case_when(
    
    ## COMPLETE ##
    
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    date_of_initial_first_contact < diag_12 & 
      end_date >= pds_12 &
         is.na(termination_or_transition_date)
    ~ "complete",
    
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    date_of_initial_first_contact < diag_12 &
      termination_or_transition_date >= pds_11
    ~ "complete",
    
    ## FAIL ##
    
    # PDS started more than 12m after diagnosis
    date_of_initial_first_contact >= diag_12
    ~ "fail",
    
    # More than 12m since diagnosis and PDS not started
    end_date >= diag_12 & 
      is.na(date_of_initial_first_contact) &
         is.na(termination_or_transition_date)
    ~ "fail",
    
    # PDS terminated before 11 months from start date
    termination_or_transition_date < pds_11 &
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail",
    
    # PDS terminated before first contact made
    is.na(date_of_initial_first_contact) & 
      !is.na(termination_or_transition_date) & 
         !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail",
    
    ## EXEMPT ##
    
    # Exempt termination reason; died/moved to other HB/refused/can't engage
    substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons
    ~ "exempt",
    
    ## ONGOING ##
    
    # Less than 12m since diagnosis and PDS not started
    end_date < diag_12 & 
      is.na(date_of_initial_first_contact) & 
         is.na(termination_or_transition_date)
    ~ "ongoing",
    
    # PDS started within 12m of diagnosis but not yet ended
    date_of_initial_first_contact < diag_12 &
      end_date < pds_12 &
         is.na(termination_or_transition_date)
    ~ "ongoing"
    
  ))


### 6 - Add time waited ----

pds %<>%
  
  # Add flag for whether record is still waiting to be seen
  # Can we use pds_status here?
  # Depends on quality of this variable
  mutate(wait_status = case_when(
    ldp == "ongoing" ~ "still waiting",
    TRUE ~ ""
  )) %>%
  
  # Number of months between diagnosis and date of first PDS contact
  mutate(
    
    time_to_start = if_else(
      !is.na(date_of_initial_first_contact),
      trunc(time_length(
        interval(dementia_diagnosis_confirmed_date, 
                 date_of_initial_first_contact), 
        "months")),
      trunc(time_length(
        interval(dementia_diagnosis_confirmed_date, 
                 end_date), 
        "months"))
    )
    
  ) %>%
  
  # Add measure of time waited/been waiting
  mutate(wait_length = case_when(
    
    between(time_to_start, 0, 3)   ~ "3 Months or Less",
    between(time_to_start, 4, 6)   ~ "4-6 Months",    
    between(time_to_start, 7, 9)   ~ "7-9 Months",
    between(time_to_start, 10, 12) ~ "10-12 Months",
    time_to_start >= 13            ~ "Over 1 Year",
    
  ))


### 8 - Create final output file ----

pds %<>%
  group_by(health_board, ijb, fy, month, ldp, wait_status, wait_length) %>%
  summarise(referrals = n()) %>%
  ungroup()

write_rds(pds, here("data", glue("{fy}-{qt}_final-data.rds")))


### END OF SCRIPT ###
