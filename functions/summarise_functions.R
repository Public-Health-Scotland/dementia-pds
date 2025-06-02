summarise_by_variable <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2 = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop")
    
  ) %>% 
    mutate(perc_met = round(((complete + exempt)/(complete + exempt + fail))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

summarise_by_variable_2 <- function(variable){
  bind_rows(
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", sex = "All", simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2 = "All", sex = "All", simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", sex = "All", simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, sex = "All", simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2, sex = "All", simd = "All",{{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2, sex = "All", simd = "All",{{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2 = "All", sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2, sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2, sex, simd = "All", {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2 = "All", sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2, sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2, sex = "All", simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2 = "All", sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb, fy, age_grp_2, sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop"),
    
    ldp %>% group_by(health_board, ijb = "All", fy, age_grp_2, sex, simd, {{variable}}) %>% 
      summarise(total_referrals = sum(n_referrals),
                complete = sum(ldp == "complete"),
                exempt = sum(ldp == "exempt"),
                fail = sum(ldp == "fail"),
                ongoing = sum(ldp == "ongoing"),
                .groups = "drop")
    
  ) %>% 
    mutate(perc_met = round(((complete + exempt)/(complete + exempt + fail))*100, 1)) %>% 
    rename(type = {{variable}})
  
}

# # simd
# summarise_by_variable_simd <- function(variable){
#   bind_rows(
#     
#     ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb, fy, sex = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb = "All", fy, sex = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb, fy, sex, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb = "All", fy, sex, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop")
#     
#   ) %>% 
#     mutate(perc_met = round(((complete + exempt)/(complete + exempt + fail))*100, 1)) %>% 
#     rename(type = {{variable}})
#   
# }

# # for gender page
# 
# summarise_by_variable_gender <- function(variable){
#   bind_rows(
#     
#     ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, simd = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb, fy, simd = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb = "All", fy, simd = "All", {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board = "Scotland", ijb = "All", fy, simd, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     ldp %>% group_by(health_board, ijb, fy, simd, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop"),
#     
#     
#     ldp %>% group_by(health_board, ijb = "All", fy, simd, {{variable}}) %>% 
#       summarise(total_referrals = sum(n_referrals),
#                 complete = sum(ldp == "complete"),
#                 exempt = sum(ldp == "exempt"),
#                 ongoing = sum(ldp == "ongoing"),
#                 fail = sum(ldp == "fail"),
#                 .groups = "drop")
#     
#   ) %>% 
#     mutate(perc_met = round(((complete + exempt)/(complete + exempt + fail))*100, 1)) %>% 
#     rename(type = {{variable}})
#   
# }

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
  
  data %<>% filter(!is.na(termination_or_transition_date), !is.na(date_of_initial_first_contact), contact_to_termination_days >= 0) 
  
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

summarise_pathways_3 <- function(data){
  
  data %<>% filter(!is.na(termination_or_transition_date), is.na(date_of_initial_first_contact)) 
  
  bind_rows(
    
    data %>% 
      group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>%  
      group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb, fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>%  
      group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason, ldp) %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>%  
      group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb, fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex, termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>%  
      group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% 
      group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason = "14 All reasons", ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex, termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb, fy, sex = "All", termination_or_transition_reason, ldp = "All") %>% 
      summarise(median_referral_to_termination = median(referral_to_termination_days, na.rm = T),
                referrals = sum(n_referrals),
                .groups = "drop")
  )
}


summarise_uptake <- function(data, field = pds_uptake_decision){
  
  data %<>% mutate(pds_uptake_decision = 
                     if_else(ijb == "Aberdeen City" & fy %in% c("2019/20","2020/21"), "Not Known", 
                             pds_uptake_decision))
  
  bind_rows(
    
     data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop")
    
  )
}

summarise_carer <- function(data, field = carers_support){
  
  bind_rows(
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2 = "All", simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2, simd = "All", {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2, simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board = "Scotland", ijb = "All", fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    data %>% group_by(health_board, ijb = "All", fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop"),
    
    
    data %>% group_by(health_board, ijb, fy, age_grp_2 = "All", simd, {{field}}) %>% 
      summarise(referrals = sum(n_referrals),
                .groups = "drop")
    
  )
}


