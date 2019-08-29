
plot_referrals <- function(data, group = 1){
  
  data %>%
    
    group_by(fy, month) %>%
    
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
               group = group,
               text = paste0(month_full, " ", year, "<br>",
                             "Referrals: ", referrals))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)) +
    
    labs(x = "",
         y = "Number of Referrals")
  
}
