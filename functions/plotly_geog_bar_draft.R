plotly_geog_bar <- function(data, x_label = "Health Board", y_value = "pop_rate_10000", y_label =  "Rate per 10,000 Population", y_value_text = "Rate per 10,000 Population: ", measure = "simd", measure_text = "SIMD Quintile: ", colours = simd_colours){
  
  plot_ly(data, x = ~geog, y = ~data[[y_value]], meta = ~fy, hovertext = ~data[[measure]], type = 'bar', color = ~data[[measure]], colors = colours, 
                            hovertemplate = paste0("%{x} <br>", "%{meta} <br>", measure_text, "%{hovertext} <br>",
                                                  y_value_text, "%{y}<extra></extra>")) %>% 
    layout(clickmode = "none", showlegend = FALSE, margin = list(l = -5, b = 10, t = 40),
           xaxis = list(title = x_label, showline = FALSE, linecolor = "#b3b3b3",
                        categoryorder = "trace", tickangle = -45, titlefont = list(size = 16)),
           yaxis = list(title = y_label, linecolor = "#b3b3b3", titlefont = list(size = 16))
    ) %>%
    config(displayModeBar = TRUE, doubleClick = F,
           modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d', 
                                         'zoomOut2d', 'autoScale2d', 
                                         'toggleSpikelines', 
                                         'hoverCompareCartesian', 
                                         'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F)
}
  
  



