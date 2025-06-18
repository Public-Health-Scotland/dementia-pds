plot_trend_2 <- function(data, measure,  x = "", y = "", colours = phs_colours_32){
  
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
    
    facet_wrap(vars(age_grp_2), ncol = 1, scales = "free_y") +
    
    scale_y_continuous(limits = c(0, NA), breaks = integer_breaks()
                         ) + 
    
    scale_colour_manual(values = {{colours}}) +
    
    theme_dementia() +
    
    labs(title = NULL,
         x = x,
         y = y,
          colour = NULL) +

    theme(axis.title.y = element_text(margin = margin(r = 15)),
      strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.spacing = unit(1000, "pt")) #this is for 'stacking facets' so only the selected one is visible
  
  
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
    layout(margin = list(l = -10, b = 60, t = 30) # to avoid labels getting cut out
           )
  
}
