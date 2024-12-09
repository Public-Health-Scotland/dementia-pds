plot_trend_prop <- function(data){
  
  plot <- data %>%  ggplot(aes(x = fy, y = total_referrals, fill = age_grp, 
                               text = paste0(geog, "<br>",
                                             fy, "<br>", 
                                             "Age Group: ", age_grp, "<br>",
                                             "Total Referrals: ", total_referrals
                                             ))) +    
    
               
          geom_col(position = position_fill()) +
    
          facet_wrap(vars(geog), nrow = 1) +
    
         scale_y_continuous(labels = scales::percent_format()) +
    
  
    labs(title = "Proportion of referrals by Age Group and Financial Year ",
         x = "Financial Year of Diagnosis",
         y = "",
         fill = NULL) +
    
     theme(strip.background = element_blank(),
         strip.text.x = element_blank(),
         legend.position = "none") + 
    
    theme(panel.spacing = unit(2000, "pt"))

  
  ggplotly(plot, tooltip = "text") %>%
     
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
    # layout(legend = list(orientation = "v")) %>% 
                          #, x = 0.5 , y = -0.4,
                           #xanchor = "right", yanchor = "right")) %>% 
     layout(margin = list(b = 50, t = 30) # to avoid labels getting cut out
          ) 
 
}

plot_trend_prop_legend <- function(data) { 
 legend <- data %>% ggplot(aes(x = fy, y = total_referrals, fill = age_grp))+
   geom_col(width = 0)+
   
  labs(fill = "Age Group") +
   
      theme(axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         legend.position = c(0.4, 0.5), # move the legend to the center
         panel.grid = element_blank(),
         panel.border = element_rect(colour = "white", fill='white', size=1)
   )
   
 legend_plotly<-ggplotly(legend, tooltip = NULL)
   
 config(legend_plotly, staticPlot = TRUE) %>%
 layout(legend = list(orientation = "v",
          x = 0, y = 0.5))  
}

