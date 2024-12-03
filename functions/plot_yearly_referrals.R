plot_yearly_referrals <- function(data){
  
  xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                      showline = TRUE, fixedrange=TRUE)
  
  yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                      tickfont = list(size=14), titlefont = list(size=14))
  
  bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                       'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                       'hoverClosestCartesian')
  
  yaxis_plots[["title"]] <- "Number of Referrals" 
  xaxis_plots[["title"]] <- "Financial Year of Diagnosis"
  
  plot <- data %>%
    
    ggplot(aes(x = fy,
               y = total_referrals,
               group = geog,
               colour = geog,
               text = paste0(geog, "<br>",
                             fy, "<br>",
                             total_referrals))) +
    
    geom_point() +
    
    geom_line() + 
    
    scale_y_continuous(limits = c(0, NA)
    ) + 
    
    #phsstyles::scale_colour_discrete_phs(palette = "all", name = NULL) +
    
    theme(legend.title = element_blank(),
    ) +
    
    labs(colour = NULL)
  
  ggplotly(plot, tooltip = "text") %>%
    
    config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.3,
                         xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(b = 30, t = 10), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots)
  
}
