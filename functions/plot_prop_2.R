plot_prop_2 <- function(data, measure, measure_text, measure_2, colours = phs_colours_32){
  
  plot <- data %>% ggplot(aes(x = geog, y = total_referrals, fill = {{measure}}, 
                               text = paste0(geog, "<br>",
                                             fy, "<br>", 
                                             measure_text, {{measure}}, "<br>",
                                             "Referrals: ", format(total_referrals, big.mark = ",")
                               ))) +    
    
    
    geom_col(position = position_fill()) +
    
    facet_wrap(vars(geog_type, {{measure_2}}), nrow = 1, scales = "free_x") +
                
    
    scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
    
    scale_fill_manual(values = colours) +
    
    labs(x = "",
         y = "",
         fill = NULL) +
    
    theme_dementia(xangle = 45) +
    
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 13,
                                      #face = "bold",
                                      margin = margin(t = 30))
    ) + 
    
   theme(panel.spacing = unit(100000, "pt")) #this is for 'stacking facets' so only the selected one is visible
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(margin = list(b = 160, t = 40) # to avoid labels getting cut out
    )
  
}


plot_prop_2_label <- function(data) { 
  legend <- data %>% ggplot(aes(x = x, y = y, fill = geog_type_2))+
    geom_col(width = 0)+
    
    labs(fill = NULL) +
    
    scale_fill_manual(values = c("white", "white")) +
    
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0, 1), 
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(size = 13
                                     #, face = "bold"
                                     )
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "h",
                         x = 0, y = 1, xanchor = "center", yanchor = "top") 
    )  
}



