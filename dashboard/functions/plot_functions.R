#### plot functions #####

theme_dementia_dashboard <- function(){
  theme_gray() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#d9d9d9"),
          axis.text = element_text(size = 10),
          axis.line = element_line()
    )
}

# yearly trend plot function for referrals and median wait times
plot_trend <- function(data, measure, group = ijb, ytitle = ""){
  
  yaxis_plots[["title"]] <- ytitle
  xaxis_plots[["title"]] <- "Financial Year of Diagnosis"
  
    
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = {{group}},
               colour = {{group}},
               text = paste0({{group}}, "<br>",
                             fy, "<br>",
                             {{measure}}))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)
                    ) + 
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.3,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 50), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}

# yearly trend plot function for percentages
plot_trend_perc <- function(data, measure, group = ijb){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- "Financial Year of Diagnosis"
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = {{group}},
               colour = {{group}},
               text = paste0(ijb, "<br>",
                             fy, "<br>",
                             {{measure}}, "%"))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, 100),
                       labels=function(x) paste0(x,"%")) + 
    
    phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.3,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}


# bar chart for ldp percentage and proportions

percent_bar_chart <- function(data, category, measure, x_text_angle = 45, legend = "none", fill = NULL, limit = 100){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter({{category}} != "Unknown", {{category}} != "Not Specified") 
  
  plot <-  data %>% ggplot(aes(x = {{category}}, y = {{measure}}, fill = {{fill}},
                               text = paste0({{category}}, "<br>",
                                             round({{measure}},1), "%"))) +
    geom_col(position=position_identity()) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, limit),
                       labels=function(x) paste0(x,"%")) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
          legend.position = legend,
          axis.text.x = element_text(angle=x_text_angle),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}



#bar chart for pathways
plot_bar_median<- function(data){
  
  yaxis_plots[["title"]] <- "Median Wait (days)"
  xaxis_plots[["title"]] <- ""
  
  plot <-  data %>%  ggplot() +
    
    geom_col(aes(x = geog, y = median_diagnosis_to_contact,
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               "Average (median) days from diagnosis to first contact: ", median_diagnosis_to_contact)),
             #position = position_dodge(),
             fill = "#0078D4") +
    
    geom_hline(aes(yintercept = scot_median_diagnosis_to_contact, 
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 "Average (median) days from diagnosis to first contact: ", scot_median_diagnosis_to_contact), color = "Scotland"), linetype = 2) +
    
    scale_color_manual(values = "#C73918") +
    
    labs(#title = "",
       #  x = "",
        # y = "Median Wait (days)",
         color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), 
      limits = c(0, NA)
                      # breaks = c(0,100,200,300,400,500,600,700,800,900,1000)
                      ) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, #doubleClick = F,
           modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(#clickmode = 'none', 
           legend = list(orientation = "h", x = 0.5 , y = -0.8,
                                             xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(#l = -10,
                         b = 30, t = 30),  # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots
    ) #%>% 
    #layout(yaxis = list(autorange = TRUE))
  
}



# # bar chart for uptake percentage
# 
# percent_uptake_bar_chart <- function(data, x_text_angle = 45, legend = "none", fill = sex){
#   
#   yaxis_plots[["title"]] <- ""
#   xaxis_plots[["title"]] <- ""
#   
#   plot <-  data %>% ggplot(aes(simd, perc_accepted, fill = {{fill}},
#                                text = paste0(simd, "<br>",
#                                              "Percentage of people that accepted PDS: ", perc_accepted, "%"))) +
#     geom_col(position=position_dodge()) +
#     
#     scale_y_continuous(limits = c(0, 100),
#                        labels=function(x) paste0(x,"%")) +
#     
#     phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
#     
#     theme(legend.title = element_blank(),
#           legend.position = legend,
#           axis.text.x = element_text(angle=x_text_angle))
#   
#   ggplotly(plot, tooltip = "text") %>%
#     
#     config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
#            displaylogo = F, editable = F) %>%
#     layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
#            yaxis = yaxis_plots, xaxis = xaxis_plots)
#   
# }
# 


# #plot monthly referrals function
# 
# plot_referrals <- function(data, 
#                            scotland = FALSE){ 
#   
#   yaxis_plots[["title"]] <- "Number"
#   xaxis_plots[["title"]] <- "Month of Diagnosis"
#   
#   # Determine whether IJBs breakdown is included
#   ijb_group <- ifelse(scotland == TRUE | 
#                         n_distinct(data$ijb) == 2, 
#                       FALSE, 
#                       TRUE)
#   
#   if(ijb_group == TRUE){
#     
#     data <- data 
#     
#   }else{
#     
#     data %<>% 
#       mutate(health_board = ifelse(scotland == TRUE, "Scotland", health_board)) %>%
#       group_by(fy, month, health_board) %>%
#       summarise(referrals = sum(referrals), .groups = "drop")
#     
#   }
#   
#   data %<>%
#     
#     mutate(year = if_else(month %in% 1:3,
#                           paste0(substr(fy, 1, 2), substr(fy, 6, 7)),
#                           substr(fy, 1, 4)),
#            month_full = month(month, label = TRUE, abbr = FALSE),
#            month_abbr = forcats::fct_relevel(
#              month(month, label = TRUE),
#              "Jan", "Feb", "Mar", after = Inf))
#   
#   plot <- data %>%
#     
#     ggplot(aes(x = month_abbr,
#                y = referrals,
#                group = if(ijb_group == TRUE){ijb}else{health_board},
#                colour = if(ijb_group == TRUE){ijb}else{health_board},
#                text = paste0(if(ijb_group == TRUE){ijb}else{health_board}, "<br>",
#                              month_full, " ", year, "<br>",
#                              "Referrals: ", format(referrals, big.mark = ",")))) +
#     geom_line() +
#     
#     geom_point() +
#     
#     scale_y_continuous(limits = c(0, NA)) +
#     
#     phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
#     
#     # Custom labels on x tick marks
#     scale_x_discrete(labels = paste(levels(data$month_abbr),
#                                     c(rep(min(data$year), 9), rep(max(data$year), 3)))) +
#     
#     theme(legend.title = element_blank(),
#           legend.position = ifelse(scotland == TRUE, "none", "bottom"),
#           axis.text.x = element_text(angle=45)) 
#   
#   
#   ggplotly(plot, tooltip = "text") %>%
#     
#     config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
#            displaylogo = F, editable = F) %>%
#     layout(legend = list(orientation = "h", x = 0.5 , y = -0.5,
#                          xanchor = "center", yanchor = "bottom")) %>% 
#     layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
#            yaxis = yaxis_plots, xaxis = xaxis_plots)
#   
#   
# }