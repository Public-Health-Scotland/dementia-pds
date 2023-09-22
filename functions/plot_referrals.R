
plot_referrals <- function(data, 
                           scotland = FALSE, 
                           quarter = NA){
  
  # Determine whether IJBs breakdown is included
  ijb_group <- ifelse(scotland == TRUE | 
                        n_distinct(data$ijb) == 1, 
                      FALSE, 
                      TRUE)
  
  # If incomplete financial year, only include complete months
  include_months <-
    if(is.na(quarter)){1:12}else{
      if(quarter == "1"){4:6}else{
        if(quarter == "2"){4:9}else{
          if(quarter == "3"){4:12}else{
            if(quarter == "4"){1:12}
          }
        }
      }
    }
  
  if(ijb_group == TRUE){
    
    data %<>% 
      group_by(fy, month, health_board, ijb) %>%
      summarise(referrals = sum(referrals), .groups = "drop")
    
    board <- 
      data %>%
        mutate(ijb = health_board) %>%
        group_by(fy, month, health_board, ijb) %>%
        summarise(referrals = sum(referrals), .groups = "drop")
    
    data %<>% 
      bind_rows(board) %>% 
      ungroup()
    
    data %<>%
      filter(!is.na(ijb)) %>%
      mutate(ijb = factor(ijb, levels = sort(unique(ijb)))) %>%
      mutate(ijb = forcats::fct_relevel(ijb, max(.$health_board)))
    
  }else{
    
    data %<>% 
      mutate(health_board = ifelse(scotland == TRUE, "Scotland", health_board)) %>%
      group_by(fy, month, health_board) %>%
      summarise(referrals = sum(referrals), .groups = "drop") %>%
      ungroup()
  
  }
  
  data %<>%

    mutate(year = if_else(month %in% 1:3,
                          paste0(substr(fy, 1, 2), substr(fy, 6, 7)),
                          substr(fy, 1, 4)),
           month_full = month(month, label = TRUE, abbr = FALSE),
           month_abbr = forcats::fct_relevel(
             month(month, label = TRUE),
             "Jan", "Feb", "Mar", after = Inf))
  
  plot <- data %>%
    
    ggplot(aes(x = month_abbr,
               y = referrals,
               group = ifelse(ijb_group, ijb, 1),
               colour = if(ijb_group == TRUE){ijb}else{health_board},
               text = paste0(if(ijb_group == TRUE){ijb}else{health_board}, "<br>",
                             month_full, " ", year, "<br>",
                             "Referrals: ", format(referrals, big.mark = ",")))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)) +
    
    scale_colour_discrete(name = NULL) +
    #phsstyles::scale_colour_discrete_phs(name = NULL) +
    
    # Custom labels on x tick marks
    scale_x_discrete(labels = paste(levels(data$month_abbr),
                                    c(rep(min(data$year), 9), rep(max(data$year), 3)))) +
    
    labs(x = "Month of Diagnosis",
         y = "Number") +
    
    theme(legend.title = element_blank(),
          legend.position = ifelse(ijb_group == FALSE, "none", "top"),
          axis.text.x = element_text(angle=45))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
    
    layout(legend = list(orientation = "h", x = 0.2 , y = -0.6,
                         xanchor = "center", yanchor = "bottom"))
  
}
