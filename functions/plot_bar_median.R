plot_bar_median<- function(data){
  
  plot <-  data %>%  ggplot() +
    
    geom_col(aes(x = geog, y = median_diagnosis_to_contact,
                 text = paste0(geog, "<br>",
                               fy, "<br>",
                               "Average (median) days from diagnosis to first contact: ", median_diagnosis_to_contact)),
             position = position_identity(), fill = "#0078D4") +
    
    geom_hline(aes(yintercept = scot_median_diagnosis_to_contact, 
                   text = paste0("Scotland", "<br>",
                                 fy, "<br>",
                                 "Average (median) days from diagnosis to first contact: ", scot_median_diagnosis_to_contact), color = "Scotland"), linetype = 2) +
    
    labs(title = "",
         x = "",
         y = "Median",
         color = NULL) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    
    scale_color_manual(values = "#C73918") +
    
    theme_dementia(xangle = 45)
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.8,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30)  # to avoid labels getting cut out
    ) 
  
}

