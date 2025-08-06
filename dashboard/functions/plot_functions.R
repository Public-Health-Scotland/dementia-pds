#### Style of x and y axis----
xaxis_plots <- list(title = FALSE, tickfont = list(size=13), titlefont = list(size=13),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=13), titlefont = list(size=13))

# Buttons to remove from plotly plots----
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 
                     'zoomOut2d', 'autoScale2d', 
                     'toggleSpikelines', 
                     'hoverCompareCartesian', 
                     'hoverClosestCartesian', 'toImage')
#theme for plots----
theme_dementia_dashboard <- function(){
  theme_gray() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#d9d9d9"),
          panel.grid.major.y = element_line(color = "#d9d9d9"),
          axis.line = element_line()
    )
}

#colours list with rust replaced with light magenta----
phs_colours_core_no_rust <- c(
  "#3F3685",
  "#9B4393",
  "#0078D4",
  "#83BB26",
  "#948DA3",
  "#1E7F84",
  "#6B5C85",
  "#CDA1C9"
)

#### PLOT FUNCTIONS #####
# yearly trend plot function for referrals and median wait times
plot_trend <- function(data, measure,
                       group = ijb, 
                       colours = c("#9B4393", "#0078D4"),
                       ytitle = ""){
  
  yaxis_plots[["title"]] <- ytitle
  xaxis_plots[["title"]] <- "Financial Year of Diagnosis"
  
  measure_values <- data %>% filter(!is.na({{measure}})) %>% 
    select({{measure}}) # used to define max value on y-axis in scale_y_continuous below
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = {{group}},
               colour = {{group}},
               text = paste0({{group}}, "<br>",
                             fy, "<br>",
                             prettyNum({{measure}}, big.mark = ",")))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(expand = c(0,0), limits = c(0, max(measure_values)+0.03*max(measure_values)) #makes y axis go slightly above largest value in selected data
    ) + 
    
    # phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    scale_colour_manual(values = {{colours}}) +
    
    labs(colour = NULL) +
    
    theme_dementia_dashboard() 
  
  # theme(legend.title = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.4,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
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
    
    scale_y_continuous(expand = c(0,0), limits = c(0, 101),
                       labels=function(x) paste0(x,"%")) + 
    
    # phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    scale_colour_manual(values = c("#9B4393","#0078D4")) +
    
    labs(colour = NULL) +
    
    theme_dementia_dashboard()
  
  # theme(legend.title = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.4,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
  
}


# bar chart for ldp percentage and proportions

plot_bar_perc <- function(data, category, measure, x_text_angle = 45, legend = "none", fill = NULL, ylimit = 101){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter({{category}} != "Unknown", {{category}} != "Not Specified") 
  
  plot <-  data %>% ggplot(aes(x = {{category}}, y = {{measure}}, fill = {{fill}},
                               text = paste0({{category}}, "<br>",
                                             round({{measure}},1), "%"))) +
    geom_col(position=position_identity()) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, ylimit),
                       labels=function(x) paste0(x,"%")) +
    
    scale_fill_manual(values = phs_colours_core_no_rust) +
    
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

# bar chart for ldp percentage and proportions

plot_bar_perc_line <- function(data, category = ijb,
                               measure, scot_measure,
                               x_text_angle = 45, legend = "none", limit = 101){
  
  yaxis_plots[["title"]] <- ""
  xaxis_plots[["title"]] <- ""
  
  data %<>% filter({{category}} != "Unknown", {{category}} != "Not Specified") 
  
  plot <-  data %>% ggplot() +
    
    geom_col((aes(x = {{category}}, y = {{measure}},
                  text = paste0({{category}}, "<br>",
                                round({{measure}},1), "%"))),
             #  position=position_identity(),
             fill = "#0078D4") +
    
    
    geom_hline(aes(yintercept = {{scot_measure}}, 
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 round({{scot_measure}},1), "%"), color = "Scotland"), linetype = 2) +
    
    scale_color_manual(values = "#9B4393") +
    
    labs(color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, limit),
                       labels=function(x) paste0(x,"%")) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
          legend.position = legend,
          axis.text.x = element_text(angle=x_text_angle),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.8,
                         xanchor = "center", yanchor = "bottom"),
           margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}



#bar chart for pathways and referrals
plot_bar <- function(data, ytitle = "Median Wait (days)",
                     measure_text = "Average (median) days from diagnosis to first contact: ", 
                     measure = median_diagnosis_to_contact,
                     scot_measure = scot_median_diagnosis_to_contact,
                     scot_measure_text = "Average (median) days from diagnosis to first contact: "){
  
  yaxis_plots[["title"]] <- ytitle
  xaxis_plots[["title"]] <- ""
  
  plot <-  data %>%  ggplot() +
    
    geom_col(aes(x = geog, y = {{measure}},
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               measure_text, {{measure}})
    ),
    #position = position_dodge(),
    fill = "#0078D4") +
    
    geom_hline(aes(yintercept = {{scot_measure}}, 
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 scot_measure_text, {{scot_measure}}), color = "Scotland"), linetype = 2) +
    
    scale_color_manual(values = "#9B4393") +
    
    labs(color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, NA)
                      ) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
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
    ) 
  
}

#bar chart for pathways and referrals
plot_bar_no_line <- function(data, ytitle,
                             measure, 
                             measure_text
){
  
  yaxis_plots[["title"]] <- ytitle
  xaxis_plots[["title"]] <- ""
  
  plot <-  data %>%  ggplot() +
    
    geom_col(aes(x = geog, y = {{measure}},
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               measure_text, {{measure}})
    ),
    fill = "#0078D4") +
    
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, NA)
                    ) +
    
    theme_dementia_dashboard() +
    
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, 
           modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.8,
                    xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(
      b = 30, t = 30),  # to avoid labels getting cut out
      yaxis = yaxis_plots, xaxis = xaxis_plots
    ) 
}



