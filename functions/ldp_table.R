
ldp_table <- function(data, 
                      scotland = FALSE,
                      include_pc = TRUE){
  
  if(scotland == TRUE){
    data %<>%
      mutate(ijb = "All") %>%
      bind_rows(data %>% 
                  mutate(health_board = "Scotland",
                         ijb          = "All")) %>%
      group_by(health_board, ijb, fy, ldp) %>%
      summarise(referrals = sum(referrals), .groups = "drop")
   }
  
  # 12 Months Complete - IJB level for table
  num_ijb <- data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb) %>% summarise(num = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt")) %>% group_by(ijb = health_board) %>% summarise(num = sum(referrals), .groups = "drop"))
  den_ijb <- data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb) %>% summarise(den = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>% filter(ldp %in% c("complete", "exempt", "fail")) %>% group_by(ijb = health_board) %>% summarise(den = sum(referrals), .groups = "drop"))
  pds_rate_ijb <- 
    full_join(num_ijb, den_ijb, by = "ijb") %>%
    mutate(rate = (num / den) * 100,
           rate = replace_na(rate, 0)) %>%
    select(-num, -den) %>%
    mutate(rate = paste0(format(round_half_up(rate, 1), big.mark = ","), "%")) %>%
    mutate(ldp = "% Met Standard/Exempt") %>%
    pivot_wider(names_from = ijb, values_from = rate)
  
  table <-
    
    data %>%
    group_by(ijb = health_board, ldp) %>%
    summarise(referrals = sum(referrals), .groups = "drop") %>%
    bind_rows(data %>%
                group_by(ijb = health_board, ldp = "total") %>%
                summarise(referrals = sum(referrals), .groups = "drop"),
              data %>%
                group_by(ijb, ldp) %>%
                summarise(referrals = sum(referrals), .groups = "drop"),
              data %>%
                group_by(ijb, ldp = "total") %>%
                summarise(referrals = sum(referrals), .groups = "drop")) %>%
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
           referrals = format(referrals, big.mark = ",")) %>%
    pivot_wider(names_from = ijb, values_from = referrals) %>%
    bind_rows(if(include_pc == TRUE)pds_rate_ijb) %>%
    select(ldp, c(data$health_board, sort(unique(data$ijb)))) %>%
    arrange(match(ldp, c("Total Referrals", "Standard Met", "Standard Not Met", "PDS Ongoing", "Exempt")))
  
  if(scotland == TRUE){
    
    table %>%
      select(-All) %>%
      
      pivot_longer(cols = c("Scotland", starts_with("NHS")),
                   names_to = "board",
                   values_to = "value") %>%
      pivot_wider(names_from = ldp,
                  values_from = value) %>%
      
      kable(col.names = c("", names(.)[-1]),
            align = c("l", rep("r", length(names(.)) - 1))
        ) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      column_spec(if_else(include_pc, 7, 6), bold = include_pc)
    
  }else{
    
    table %>%
      kable(col.names = c("", unique(data$health_board), sort(unique(data$ijb))),
            align = c("l", "r", rep("r", length(unique(data$ijb))))) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      add_header_above(
        c(" " = 1,
          "Health Board" = 1,
          "Integration Joint Board (IJB)" = length(unique(data$ijb)))) %>%
      row_spec(if_else(include_pc, 6, 5), bold = include_pc)
    
  }

  
}
