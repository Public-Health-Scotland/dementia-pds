plot_bar_pop_rate <- function(data1, data2){
  
  xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                      showline = TRUE, fixedrange=TRUE)
  
  yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                      tickfont = list(size=14), titlefont = list(size=14))
  
  bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                       'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                       'hoverClosestCartesian')
  
  yaxis_plots[["title"]] <- "rate per 10,000 popuation" 
  xaxis_plots[["title"]] <- ""
  
  
  plot <-  ggplot() +
    
    geom_col(aes(x = geog, y = pop_rate_10000,
                 text = paste0(geog, "<br>",
                               "rate per 10,000 population: ", pop_rate_10000)),
             position = position_identity(), fill = "#0078D4", data1) +
    
    geom_hline(aes(yintercept = pop_rate_10000, 
               text = paste0(geog, "<br>",
                             "rate per 10,000 population: ", pop_rate_10000), color = "Scotland"), linetype = 2, data2) +
    
    scale_colour_manual(values = "#C73918") +
    
    labs(title = "",
         x = "",
         y = "",
         color = NULL) +
    
    scale_y_continuous(limits = c(0, NA)) +
    
     theme(axis.text.x = element_text(angle=45)) 
  
  ggplotly(plot, tooltip = "text") %>%
     
     config(displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove, 
            displaylogo = F, editable = F) %>%
     layout(legend = list(orientation = "h", x = 0.5 , y = -0.4,
                           xanchor = "center", yanchor = "bottom")) %>% 
     layout(margin = list(b = 30, t = 30), # to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots) 
 
}

