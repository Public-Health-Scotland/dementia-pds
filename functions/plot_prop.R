plot_prop <- function(data, xlabel = NULL){
  
  plot <- data %>%  ggplot(aes(x = geog, y = total_referrals, fill = age_grp, 
                               text = paste0(geog, "<br>",
                                             fy, "<br>", 
                                             "Age Group: ", age_grp, "<br>",
                                             "Total Referrals: ", total_referrals
                                             ))) +    
    
               
          geom_col(position = position_fill()) +
    
          facet_wrap(vars(fy), nrow = 1) +
    
         scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
    
    phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
    
      labs(x = xlabel,
         y = "",
         fill = NULL) +
    
    theme_dementia(xangle = 40) +
    
     theme(strip.background = element_blank(),
         strip.text.x = element_blank(),
         legend.position = "none",
         axis.title.x = element_text(size = 11,
                                    face = "bold",
                                    margin = margin(t = -50))
         ) + 
    
    theme(panel.spacing = unit(700, "pt")) #this is for 'stacking facets' so only the selected one is visible

  ggplotly(plot, tooltip = "text") %>%
     
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    # layout(legend = list(orientation = "v")) %>% 
                          #, x = 0.5 , y = -0.4,
                           #xanchor = "right", yanchor = "right")) %>% 
     layout(margin = list(b = 150, t = 40) # to avoid labels getting cut out
           )
 
}

plot_prop_legend <- function(data) { 
 legend <- data %>% ggplot(aes(x = geog, y = total_referrals, fill = age_grp))+
   geom_col(width = 0)+
   
  labs(fill = "Age Group:") +
   
   phsstyles::scale_fill_discrete_phs(palette = "all", name = NULL) +
   
      theme(axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         legend.position = c(0.4, 0.5), # move the legend to the center
         panel.grid = element_blank(),
         panel.border = element_rect(colour = "white", fill='white', size=1)
   )
   
 legend_plotly<-ggplotly(legend, tooltip = NULL)
   
 config(legend_plotly, staticPlot = TRUE) %>%
   layout(legend = list(orientation = "h",
                        x = 0.5, y = 1, xanchor = "center", yanchor = "top" ) 
   )  
}

