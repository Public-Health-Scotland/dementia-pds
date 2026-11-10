plot_bar_mi <- function(data, line_data, x, y){
  
  y_name <- rlang::as_name(rlang::ensym(y))
  
  labels <- switch(
    y_name,
    "rate" = list(
      axis = "Rate per 10,000 population",
      tooltip = "Rate per 10,000 population: "
    ),
    "diagnosis_to_contact" = list(
      axis = "Median Wait (days)",
      tooltip = "Average (median) days from diagnosis to first contact: "
    ),
    stop("Unknown measure: ", y_name)
  )
  
  axis_label <- labels$axis
  tooltip_label <- labels$tooltip
  
  plot <- ggplot() +    
    geom_col(
      data = data,
      aes(
        x = {{x}}, 
        y = {{y}}, 
        text = paste0({{x}}, "<br>", fy, "<br>", tooltip_label, {{y}})), 
      position = position_identity(), 
      fill = "#0078D4") +
    geom_hline(
      data = line_data,
      aes(
        yintercept = {{y}}, 
        text = paste0({{x}}, "<br>", fy, "<br>", tooltip_label, {{y}}), 
        color = "Scotland"), 
      linetype = 2) +
    scale_colour_manual(values = "#C73918") +
    labs(
      title = "",
      x = "",
      y = axis_label,
      color = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_dementia(xangle = 45)
  
  ggplotly(plot, tooltip = "text") %>%
    config(
      displayModeBar = TRUE, 
      doubleClick = F,
      modeBarButtonsToRemove = list(
        'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 
        'autoScale2d', 'toggleSpikelines', 'hoverCompareCartesian', 
        'hoverClosestCartesian', 'toImage'), 
      displaylogo = F, editable = F) %>%
    layout(legend = list(orientation = "h", x = 0.5 , y = -0.7, xanchor = "center", yanchor = "bottom")) %>% 
    layout(margin = list(l = -10, b = 30, t = 30)) # to avoid labels getting cut out 
}
