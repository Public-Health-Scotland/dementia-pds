#### plot functions #####

#plot monthly referrals function

plot_referrals <- function(data, 
                           scotland = FALSE){ 
  
  yaxis_plots[["title"]] <- "Number"
  xaxis_plots[["title"]] <- "Month of Diagnosis"
  
  # Determine whether IJBs breakdown is included
  ijb_group <- ifelse(scotland == TRUE | 
                        n_distinct(data$ijb) == 2, 
                      FALSE, 
                      TRUE)
  
  if(ijb_group == TRUE){
    
    data <- data 
    
  }else{
    
    data %<>% 
      mutate(health_board = ifelse(scotland == TRUE, "Scotland", health_board)) %>%
      group_by(fy, month, health_board) %>%
      summarise(referrals = sum(referrals), .groups = "drop")
    
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
               group = if(ijb_group == TRUE){ijb}else{health_board},
               colour = if(ijb_group == TRUE){ijb}else{health_board},
               text = paste0(if(ijb_group == TRUE){ijb}else{health_board}, "<br>",
                             month_full, " ", year, "<br>",
                             "Referrals: ", format(referrals, big.mark = ",")))) +
    geom_line() +
    
    geom_point() +
    
    scale_y_continuous(limits = c(0, NA)) +
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    # Custom labels on x tick marks
    scale_x_discrete(labels = paste(levels(data$month_abbr),
                                    c(rep(min(data$year), 9), rep(max(data$year), 3)))) +
    
    theme(legend.title = element_blank(),
          legend.position = ifelse(scotland == TRUE, "none", "bottom"),
          axis.text.x = element_text(angle=45)) 
  
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.5,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}

# yearly trend plot function for percentages
plot_trend <- function(data, measure){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>%
    
    select(ijb, fy, {{measure}}) %>%
    filter(!is.na({{measure}})) %>% 
    distinct(ijb, fy, .keep_all = T)
  
  
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = ijb,
               colour = ijb,
               text = paste0(ijb, "<br>",
                             fy, "<br>",
                             {{measure}}, "%"))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) + 
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.2,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}

# yearly trend plot function for referrals
plot_trend_referrals <- function(data, measure){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>%
    
    select(ijb, fy, {{measure}}) %>%
    filter(!is.na({{measure}})) %>% 
    distinct(ijb, fy, .keep_all = T)
  
  
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = ijb,
               colour = ijb,
               text = paste0(ijb, "<br>",
                             fy, "<br>",
                             {{measure}}))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)
                    ) + 
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.2,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}

# bar chart for proportion of referrals

proportion_bar_chart <- function(data, x_text_angle = 45, legend = "none", fill = sex){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter(type != "Unknown", type != "Not Specified") 
  
  plot <-  data %>% ggplot(aes(x = type, y = total_referrals/sum(total_referrals)*100, fill = {{fill}},
                               text = paste0(type, "<br>",
                                             "Proportion of total referrals: ", round(total_referrals/sum(total_referrals)*100,1), "%"))) +
    geom_col(position=position_dodge()) +
    
    scale_y_continuous(limits = c(0, NA),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
          legend.position = legend,
          axis.text.x = element_text(angle=x_text_angle))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}

# bar chart for ldp percentage

percent_met_bar_chart <- function(data, x_text_angle = 45, legend = "none", fill = sex){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter(type != "Unknown", type != "Not Specified") 
  
  plot <-  data %>% ggplot(aes(type, percent_met, fill = {{fill}},
                               text = paste0(type, "<br>",
                                             "Percentage of Referrals Achieved LDP Standard: ", percent_met, "%"))) +
    geom_col(position=position_dodge()) +
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
          legend.position = legend,
          axis.text.x = element_text(angle=x_text_angle))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}


# bar chart for uptake percentage

percent_uptake_bar_chart <- function(data, x_text_angle = 45, legend = "none", fill = sex){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  plot <-  data %>% ggplot(aes(simd, perc_accepted, fill = {{fill}},
                               text = paste0(simd, "<br>",
                                             "Percentage of patients that accepted PDS: ", perc_accepted, "%"))) +
    geom_col(position=position_dodge()) +
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
          legend.position = legend,
          axis.text.x = element_text(angle=x_text_angle))
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}
