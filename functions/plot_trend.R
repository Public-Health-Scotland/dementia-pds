plot_trend <- function(data, measure){
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = geog,
               colour = geog,
               text = paste0(geog, "<br>",
                             fy, "<br>",
                             {{measure}}))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA), breaks = integer_breaks()
                         ) + 
    
    #phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    labs(title = NULL,
         x = "",
         y = "",
         colour = NULL)
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.2,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 30) # to avoid labels getting cut out
           )
  
}
