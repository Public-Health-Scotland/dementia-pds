summarise_by_variable <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete_exempt = sum(ldp == "complete" | ldp == "exempt"),
                total_minus_ongoing = sum(ldp != "ongoing"),  .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete_exempt = sum(ldp == "complete" | ldp == "exempt"),
                total_minus_ongoing = sum(ldp != "ongoing"),  .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete_exempt = sum(ldp == "complete" | ldp == "exempt"),
                total_minus_ongoing = sum(ldp != "ongoing"),  .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete_exempt = sum(ldp == "complete" | ldp == "exempt"),
                total_minus_ongoing = sum(ldp != "ongoing"),  .groups = "drop")
  
   ) %>% 
    mutate(percent_met = round(complete_exempt/total_minus_ongoing*100, 1)) %>% 
    rename(type = {{variable}})
  
}

#for demographics tab

summarise_by_variable_demo <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop")
    
  ) %>% 
    mutate(percent_met = round(((complete + exempt)/(total_referrals - ongoing))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

# for gender page

summarise_by_variable_gender <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", fy, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", fy, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, fy, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop")
    
  ) %>% 
    mutate(percent_met = round(((complete + exempt)/(total_referrals - ongoing))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

# pathways (waiting times)

summarise_pathways <- function(data){
  
  bind_rows(
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              allocated_referrals = sum(!is.na(initial_pds_practitioner_allocation_date)),
              contacted_referrals = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop")
  )
}


summarise_pathways_2 <- function(data){
  
  data %<>% filter(!is.na(termination_or_transition_date), !is.na(date_of_initial_first_contact)) 
  
  bind_rows(
    
    data %>% filter(ldp != "exempt") %>% 
      group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
               .groups = "drop"),
    
    data %>% filter(ldp != "exempt") %>% 
        group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% filter(ldp != "exempt") %>% 
      group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% filter(ldp != "exempt") %>%  
      group_by(health_board, ijb, fy, sex, termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex, termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% filter(ldp != "exempt") %>%  
      group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% filter(ldp != "exempt") %>% 
      group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason = "All reasons") %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop")
  )
}


