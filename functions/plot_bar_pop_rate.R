plot_bar_pop_rate <- function(bar_data, line_data, x, y){
  
  plot <- ggplot() +    
    geom_col(data = bar_data,
             aes(x = {{x}}, 
                 y = {{y}}, 
                 text = paste0({{x}}, "<br>", fy, "<br>", "Rate per 10,000 population: ", {{y}})), 
             position = position_identity(), 
             fill = "#0078D4") +
    geom_hline(data = line_data,
               aes(yintercept = {{y}}, 
                   text = paste0({{x}}, "<br>", fy, "<br>", "Rate per 10,000 population: ", {{y}}), 
                   color = "Scotland"), 
               linetype = 2) +
    scale_colour_manual(values = "#C73918") +
    labs(title = "",
         x = "",
         y = "Rate per 10,000 Popuation",
         color = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_dementia(xangle = 45)
  
  ggplotly(plot, tooltip = "text") %>%
    config(displayModeBar = TRUE, 
           doubleClick = F,
           modeBarButtonsToRemove = list(
             'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 
             'autoScale2d', 'toggleSpikelines', 'hoverCompareCartesian', 
             'hoverClosestCartesian', 'toImage'), 
           displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.7, xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30)) # to avoid labels getting cut out 
}

