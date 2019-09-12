### 6 - Add old LDP standard classification ----

pds %<>%
  
  ## FAIL ##
  
  mutate(ldp_old = case_when(
    
    # Wait time longer than 12 months
    trunc(time_length(
      interval(dementia_diagnosis_confirmed_date, 
               date_of_initial_first_contact), 
      "months")) > 12 ~ "fail",
    
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