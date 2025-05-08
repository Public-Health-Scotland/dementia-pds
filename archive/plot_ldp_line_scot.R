plot_ldp_line_scot <- function(data, measure, measure_text, ncol = 5, nrow = NULL, colours = phs_colours_core){
  
  plot <-  ggplot(data) +
    
    geom_point(aes(x = fy, y = perc_met, group = {{measure}}, colour = {{measure}},
                   text = paste0(geog, "<br>",
                                 fy, "<br>",
                                 measure_text, {{measure}}, "<br>",
                                 paste0(perc_met, "%"))
    )
    ) +
    
    geom_line(aes(x = fy, y = perc_met, group = {{measure}}, colour = {{measure}}))+
    
    labs(title = "",
         x = "",
         y = "",
         colour = NULL,
         group = NULL,
    ) +
    
    scale_y_continuous(limits = c(0, NA),  labels=function(x) paste0(x,"%")
    ) +
    
    scale_x_discrete(breaks = everyother
    ) +
    
    scale_colour_manual(values = colours) +
    
    
    facet_wrap(vars({{measure}}), ncol = ncol, nrow = nrow) + 
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_rect(fill = "#DFDDE3"),
          strip.text.x = element_text(size = 9),
          legend.position = "none"
    ) 
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.6 , y = 1.1,
                         xanchor = "center", yanchor = "top")) %>% 
    layout(margin = list(l = -5, b = 50, t = 40) # to avoid labels getting cut out
    ) 
  
}



