plot_bar_pop_rate_2 <- function(data, geogs = 14){
  
  plot <-  data %>% ggplot() +
    
    geom_col(aes(x = geog, y = pop_rate_10000, fill = geog,
                 text = paste0(geog, "<br>",
                               "rate per 10,000 population: ", pop_rate_10000)),
             position = position_identity()) +
    
    facet_wrap(vars(age_grp_2), ncol = 1, scales = "free_y") +
    
    labs(title = "",
         x = "",
         y = "Rate per 10,000 Popuation",
         color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    
    scale_fill_manual(values = c("#3F3685", rep("#0078D4", geogs))) +
    
    theme_dementia(xangle = 45) +
    
      theme(axis.title.y = element_text(margin = margin(r = 10)),
            legend.position = "none",
            strip.background = element_blank(),
            strip.text.x = element_blank(),
           panel.spacing = unit(700, "pt")) #this is for 'stacking facets' so only the selected one is visible

  
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
    layout(margin = list(l = -10, b = 30, t = 30) # to avoid labels getting cut out
    ) 
  
}

