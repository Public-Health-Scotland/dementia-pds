plot_bar_pop_rate <- function(data){
  
  plot <-  data %>%  ggplot() +
    
    geom_col(aes(x = geog, y = pop_rate_10000,
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               "Rate per 10,000 population: ", pop_rate_10000)),
             position = position_identity(), fill = "#0078D4") +
    
      geom_hline(aes(yintercept = scot_pop_rate_10000, 
                    text = paste0("Scotland", "<br>",
                                  fy, "<br>",
                                  "Rate per 10,000 population: ", scot_pop_rate_10000), color = "Scotland"), linetype = 2) +
    
    labs(title = "",
         x = "",
         y = "Rate per 10,000 Popuation",
         color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    
   scale_color_manual(values = "#C73918") +
    
    theme_dementia(xangle = 45)
    
       ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.7,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30)  # to avoid labels getting cut out
    ) 
  
}

