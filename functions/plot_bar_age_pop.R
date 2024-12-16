plot_bar_age_pop <- function(data){
  
  plot <-  data %>% ggplot() +
    
    geom_col(aes(x = age_grp, y = scot_pop_rate_10000,
                 text = paste0("Scotland", "<br>",
                               fy, "<br>",
                               "rate per 10,000 population: ", scot_pop_rate_10000)),
             position = position_identity(), just = 0, width = 0.42, fill = "#0078D4") +
    
    geom_col(aes(x = age_grp, y = pop_rate_10000,
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               "rate per 10,000 population: ", pop_rate_10000)),
             position = position_identity(), just = 1, width = 0.42, fill = "#3F3685") +
    
    labs(title = "",
         x = "Age Group",
         y = "Rate per 10,000 Popuation",
         ) +
    
    scale_y_continuous(limits = c(0, NA)) +
    
   facet_wrap(vars(fy), scales = "free_y", nrow = 1) + 
    
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 11,
                                      face = "bold",
                                      margin = margin(t = 7))
          ) +
    
    theme(panel.spacing = unit(700, "pt")) #this is for 'stacking' facets so only the selected one is visible
    
   # theme(axis.text.x = element_text(angle=45)) 
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
  #  layout(legend = list(orientation = "v", x = 0.5 , y = -0.4,
               #          xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -15, b = 50, t = 40) # to avoid labels getting cut out
    ) 

}

plot_bar_age_pop_legend <- function(data) { 
  legend <- data %>% ggplot(aes(y = y, fill = geog))+
    geom_bar(width = 0) +
    
      scale_fill_manual(name=NULL,
                        values=c("#0078D4","#3F3685")) +
    

    
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0, 0.5), # move the legend to the center
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "white", fill='white', size=1)
    )
  
  legend_plotly<-ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(orientation = "v",
                         x = -2, y = 0.5))  
}

