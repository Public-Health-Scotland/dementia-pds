
ldp_table <- function(data, data_scot){
  
  scot <-
    if(!is.null(data_scot)){
      data_scot %>%
        group_by(ldp) %>%
        summarise(referrals = sum(referrals)) %>%
        mutate(region = "Scotland")
    }
  
  ijb <-
    if(n_distinct(data$ijb) > 1){
      data %>%
        group_by(ijb, ldp) %>%
        summarise(referrals = sum(referrals)) %>%
        rename(region = ijb)
    }
  
  table_data <-
    
    data %>%
    group_by(health_board, ldp) %>%
    summarise(referrals = sum(referrals)) %>%
    rename(region = health_board) %>%
    ungroup() %>%
    
    bind_rows(scot, ijb) %>%
    
    spread(ldp, referrals) %>%
    
    mutate(region = fct_relevel(as_factor(region),
                                "Scotland",
                                max(data$health_board),
                                after = 0)) %>%
    arrange(region)
  
  return(table_data)
  
}
