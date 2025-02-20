summarise_by_variable_dashboard <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop")
    
  ) %>% 
    mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1)) %>% 
    rename(type = {{variable}})
  
}


# simd
summarise_by_variable_simd_dashboard <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, sex = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, sex, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop")
    
  ) %>% 
    mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

# for gender page

summarise_by_variable_gender_dashboard <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
   
    ldp %>% group_by(health_board, ijb, fy, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop"),
    
    
    ldp %>% group_by(health_board, ijb = "All", fy, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                ongoing = sum(ldp == "ongoing"),
                not_met = sum(ldp == "fail"),
                .groups = "drop")
    
  ) %>% 
    mutate(percent_met = round(((complete + exempt)/(complete + exempt + not_met))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

# pathways (waiting times)

summarise_pathways_dashboard <- function(data){
  
  bind_rows(
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex = "All", simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex, simd = "All") %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex, simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop"),
  
  data %>% group_by(health_board, ijb, fy, sex = "All", simd) %>% 
    summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
              median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
              median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
              median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
              median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
              total_referrals = sum(n_referrals),
              referrals_allocated = sum(!is.na(initial_pds_practitioner_allocation_date)),
              referrals_contacted = sum(!is.na(date_of_initial_first_contact)),
              .groups = "drop")
  )
}


summarise_pathways_2_dashboard <- function(data){
  
  data %<>% filter(!is.na(termination_or_transition_date), !is.na(date_of_initial_first_contact))#, ldp != "Aberdeen") 
  
  bind_rows(
    
    data %>% 
      group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
               .groups = "drop"),
    
    data %>% 
        group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>%  
      group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb, fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
   data %>%  
      group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
   data %>% 
      group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
   
   data %>% 
     group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% 
     group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>%  
     group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% 
     group_by(health_board, ijb, fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board, ijb, fy, sex, termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>%  
     group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% 
     group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop"),
   
   data %>% group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
     summarise(median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
               referrals = sum(n_referrals),
               .groups = "drop")
  )
}


summarise_uptake_dashboard <- function(data){
  
  bind_rows(
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, sex = "All", simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, sex, simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, sex, simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, sex, simd = "All", pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, sex, simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, sex = "All", simd, pds_uptake_decision) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop")
    
  )
}

