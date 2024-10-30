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