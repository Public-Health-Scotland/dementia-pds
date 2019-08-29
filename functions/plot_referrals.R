
plot_referrals <- function(data, ijb_group = TRUE){
  
  if(ijb_group == TRUE){
    data %<>% group_by(fy, month, ijb)
  }else{
    data %<>% group_by(fy, month)
  }
  
  data %>%
    
    summarise(referrals = sum(referrals)) %>%
    
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
               text = paste0(month_full, " ", year, "<br>",
                             "Referrals: ", referrals))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)) +
    
    labs(x = "",
         y = "Number of Referrals")
  
}
