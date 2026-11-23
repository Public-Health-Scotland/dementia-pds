plot_bar_legend <- function(data, measure, measure_text = "") {
  
  measure_name <- rlang::as_name(rlang::ensym(measure))
  colours <- if (measure_name == "simd") {
    c("#0062AD", "#0078D4", "#3393DD", "#80BCEA", "#B3D7F2")
  } else if (measure_name == "sex") {
    c("#3F3685", "#9B4393")
  } else if (measure_name == "age_grp_2") {
    c("#6B5C85", "#0078D4", "#1E7F84")
  }
  
  legend <- ggplot(data, aes(x = {{measure}}, y = 0, group = {{measure}}, fill = {{measure}})) +
    geom_col() +
    labs(fill = measure_text) +
    scale_fill_manual(values = colours) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position.inside = c(0.5, 0.5),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "white", fill='white', linewidth =0))
  
  legend_plotly <- ggplotly(legend, tooltip = NULL)
  
  config(legend_plotly, staticPlot = TRUE) %>%
    layout(legend = list(
      orientation = "h", x = 0.5, y = 0.5, xanchor = "center", yanchor = "right" )
    )  
}
