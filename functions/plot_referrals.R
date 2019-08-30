
plot_referrals <- function(data, scotland = FALSE){
  
  # Determine whether IJBs breakdown is included
  ijb_group <- ifelse(scotland == TRUE | 
                        n_distinct(data$ijb) == 1, 
                      FALSE, 
                      TRUE)
  
  if(ijb_group == TRUE){
    
    data %<>% 
      group_by(fy, month, health_board, ijb) %>%
      summarise(referrals = sum(referrals))
    
    board <- 
      data %>%
        mutate(ijb = health_board) %>%
        group_by(fy, month, health_board, ijb) %>%
        summarise(referrals = sum(referrals))
    
    data %<>% bind_rows(board)
    
  }else{
    
    data %<>% 
      mutate(health_board = ifelse(scot == TRUE, "Scotland", health_board)) %>%
      group_by(fy, month, health_board) %>%
      summarise(referrals = sum(referrals))
  
  }
  
  data %<>%
    
    mutate(year = if_else(month %in% 1:3,
                          paste0(substr(fy, 1, 2), substr(fy, 6, 7)),
                          substr(fy, 1, 4)),
           month_full = month(month, label = TRUE, abbr = FALSE),
           month_abbr = forcats::fct_relevel(
             month(month, label = TRUE),
             "Jan", "Feb", "Mar", after = Inf)) %>%
    
    ggplot(aes(x = month_abbr,
               y = referrals,
               group = if(ijb_group == TRUE){ijb}else{1},
               colour = if(ijb_group == TRUE){ijb}else{NULL},
               text = paste0(if(ijb_group == TRUE){ijb}else{health_board}, "<br>",
                             month_full, " ", year, "<br>",
                             "Referrals: ", referrals))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)) +
    
    labs(x = "",
         y = "Number of Referrals")
  
}
