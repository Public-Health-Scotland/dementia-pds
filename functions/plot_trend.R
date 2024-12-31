plot_trend <- function(data, measure,  x = "", y = "", colours = phs_colours_32){
  
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
    
    scale_colour_manual(values = {{colours}}) +
    
    theme_set(theme_minimal(base_size = 12)) +
    
    theme(
      axis.ticks = element_line(color = "grey92"),
      axis.line = element_line(colour = "grey70"),
      # example of adjusting axis text
      # axis.text.x = element_text(size = 10, angle = 50, vjust = 0.5, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.background = element_blank()
    ) +
    
    labs(title = NULL,
         x = x,
         y = y,
         colour = NULL)
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.35,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30) # to avoid labels getting cut out
           )
  
}
