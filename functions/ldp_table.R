
ldp_table <- function(data, 
                      include_pc = TRUE){
  
  # 12 Months Complete - IJB level for table
  num_ijb <- data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb) %>% summarise(num = sum(referrals)) %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb = health_board) %>% summarise(num = sum(referrals)))
  den_ijb <- data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb) %>% summarise(den = sum(referrals)) %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb = health_board) %>% summarise(den = sum(referrals)))
  pds_rate_ijb <- 
    full_join(num_ijb, den_ijb) %>%
    mutate(rate = (num / den) * 100,
           rate = replace_na(rate, 0)) %>%
    select(-num, -den) %>%
    mutate(rate = paste0(round_half_up(rate, 1), "%")) %>%
    mutate(ldp = "% received 12 months PDS") %>%
    pivot_wider(names_from = ijb, values_from = rate)
  
  table <-
    
    data %>%
  
    group_by(ijb = health_board, ldp) %>%
    summarise(referrals = sum(referrals)) %>%
    bind_rows(data %>%
                group_by(ijb = health_board, ldp = "total") %>%
                summarise(referrals = sum(referrals)),
              data %>%
                group_by(ijb, ldp) %>%
                summarise(referrals = sum(referrals)),
              data %>%
                group_by(ijb, ldp = "total") %>%
                summarise(referrals = sum(referrals))) %>%
    ungroup() %>%
    complete(ijb, ldp = c("complete", "fail", "exempt", "ongoing", "total"),
             fill = list(referrals = 0)) %>%
    mutate(ldp = 
             case_when(
               ldp == "complete" ~ "Standard Met",
               ldp == "fail" ~ "Standard Not Met",
               ldp == "ongoing" ~ "PDS Ongoing",
               ldp == "total" ~ "Total Referrals",
               TRUE ~ str_to_title(ldp)
             )) %>%
    mutate(referrals = replace_na(referrals, 0),
           referrals = as.character(referrals)) %>%
    pivot_wider(names_from = ijb, values_from = referrals) %>%
    bind_rows(if(include_pc == TRUE)pds_rate_ijb) %>%
    select(ldp, c(data$health_board, sort(unique(data$ijb)))) %>%
    arrange(match(ldp, c("Total Referrals", "Standard Met", "Standard Not Met", "PDS Ongoing", "Exempt")))
  
  if(include_pc == TRUE){
    
    table %>%
    kable(col.names = c("", unique(data$health_board), sort(unique(data$ijb))),
          align = c("l", "r", rep("r", length(unique(data$ijb))))) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      row_spec(6, bold = T) %>%
      add_header_above(
        c(" " = 1,
          "Health Board" = 1,
          "Integration Joint Board (IJB)" = length(unique(data$ijb))))
    
  }else{
    
    table %>%
    kable(col.names = c("", unique(data$health_board), sort(unique(data$ijb))),
          align = c("l", "r", rep("r", length(unique(data$ijb))))) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      add_header_above(
        c(" " = 1,
          "Health Board" = 1,
          "Integration Joint Board (IJB)" = length(unique(data$ijb))))
    
  }
  
}
