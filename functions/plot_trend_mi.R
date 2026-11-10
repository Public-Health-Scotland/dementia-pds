plot_trend_mi <- function(data,
                       measure,
                       x = "",
                       y = "",
                       colours = phs_colours_core,
                       measure_text = "",
                       Scotland = FALSE,
                       percentage = FALSE) {
  
  plot <- data %>%
    ggplot(aes(
      x = fy,
      y = {{ measure }},
      group = geog,
      colour = geog,
      text = if (percentage) {
        paste0(
          geog, "<br>",
          fy, "<br>",
          {{ measure }}, "%"
        )
      } else {
        paste0(
          geog, "<br>",
          fy, "<br>",
          measure_text,
          format({{ measure }}, big.mark = ",")
        )
      }
    )) +
    geom_point() +
    geom_line() +
    scale_colour_manual(values = colours) +
    theme_dementia() +
    labs(
      title = NULL,
      x = x,
      y = y,
      colour = NULL
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = if (Scotland) "none" else "bottom"
    )
  
  if (percentage) {
    
    plot <- plot +
      scale_y_continuous(
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")
      )
    
  } else {
    
    plot <- plot +
      scale_y_continuous(
        limits = c(0, NA),
        breaks = integer_breaks()
      )
  }
  
  ggplotly(plot, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = list(
        "select2d", "lasso2d", "zoomIn2d",
        "zoomOut2d", "autoScale2d",
        "toggleSpikelines",
        "hoverCompareCartesian",
        "hoverClosestCartesian",
        "toImage"
      ),
      displaylogo = FALSE,
      editable = FALSE
    ) %>%
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.5,
        xanchor = "center",
        yanchor = "bottom"
      ),
      margin = list(
        l = -10,
        b = 30,
        t = 30
      )
    )
}