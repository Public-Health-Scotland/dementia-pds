plot_legend_demog  <- function(data) {
  
  legend_plot <- data %>% 
    ggplot(aes(x = x, y = y, linetype = legend)) +
    geom_line()+
    scale_linetype_manual(values = c(c(rep("solid", 45), "dashed"))) +
    labs(fill = NULL, linetype = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = c(0.5, 0.5),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "white", fill='white', size=0))
  
  ggplotly(legend_plot, tooltip = NULL) %>%
    config(staticPlot = TRUE) %>%
    layout(legend = list(
      orientation = "h",
      x = 0.5, 
      y = 0.5, 
      xanchor = "center", 
      yanchor = "right" ))
}
