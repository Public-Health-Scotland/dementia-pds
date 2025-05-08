plot_trend <- function(data, measure,  x = "", y = "", colours = phs_colours_core, measure_text, Scotland = FALSE){
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = {{measure}},
               group = geog,
               colour = geog,
               text = paste0(geog, "<br>",
                             fy, "<br>",
                             measure_text, format({{measure}}, big.mark = ",")))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA), breaks = integer_breaks()
                         ) + 
    
    scale_colour_manual(values = {{colours}}) +
    
    theme_dementia() +
    
    labs(title = NULL,
         x = x,
         y = y,
         colour = NULL)   +
    
    theme(legend.title = element_blank(),
          legend.position = ifelse(Scotland == TRUE, "none", "bottom")
    )
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.5,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30) # to avoid labels getting cut out
           )
  
}
