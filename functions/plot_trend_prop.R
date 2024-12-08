plot_trend_prop <- function(data){
  
  plot <-  ggplot() +         
               
          geom_col(aes(x = fy, y = total_referrals, fill = age_grp), position = position_fill(), data) +
                         #text = paste0(geog, "<br>", "rate per 10,000 population: ", pop_rate_10000)
    
             scale_y_continuous(labels = scales::percent_format()) +
    
     facet_wrap(vars(geog), nrow = 1, ncol = 1) +
    
    labs(title = "Proportion of referrals by Age Group and Financial Year ",
         x = "",
         y = "") 
    
     theme(legend.position = "right") 
  
  ggplotly(plot) %>% #, tooltip = "text") %>%
     
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian'), 
           displaylogo = F, editable = F) %>%
     #layout(legend = list(orientation = "v") %>% 
                          #, x = 0.5 , y = -0.4,
                           #xanchor = "right", yanchor = "right")) %>% 
     layout(margin = list(b = 30, t = 30) # to avoid labels getting cut out
          ) 
 
}


