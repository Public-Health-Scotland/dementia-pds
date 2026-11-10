plot_trend_demog <- function(
    data,
    demog,
    measure,
    scales = NULL,
    facet_space = 6) {
  
  # Set colours and demographic label
  if (demog == "age_grp_2") {
    colours <- c("#6B5C85", "#0078D4", "#1E7F84")
    demog_text <- "Age Group: "
  } else if (demog == "sex") {
    colours <- c("#3F3685", "#9B4393")
    demog_text <- "Gender: "
  } else if (demog == "simd") {
    colours <- c("#0062AD", "#0078D4", "#3393DD", "#80BCEA", "#B3D7F2")
    demog_text <- "Deprivation Quintile: "
  } else {
    colours <- phs_colours_core
    demog_text <- paste0(demog, ": ")
  }
  
  # Set measure label / tooltip text
  if (measure == "rate") {
    measure_text <- "Rate per 10,000 Population: "
  } else {
    measure_text <- ""
  }
  
  plot <- ggplot(data, aes(
    x = fy,
    y = .data[[measure]],
    group = geog,
    linetype = if_else(geog == "Scotland", "Scotland", "Other"),
    colour = .data[[demog]])) +
    geom_point(aes(
      text = paste0(
        geog, "<br>", fy, "<br>", demog_text, .data[[demog]], "<br>", measure_text, 
        round(.data[[measure]], 1), if_else(measure == "perc_met", "%", "")))) +
    geom_line() +
    labs(title = "", x = "", y = str_replace(measure_text, ": ", ""), 
         colour = NULL, linetype = NULL) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_discrete(breaks = everyother) +
    scale_colour_manual(values = colours) +
    scale_linetype_manual(values = c(
      "Scotland" = "dashed",
      "Other" = "solid")) +
    facet_wrap(vars(.data[[demog]]), ncol = 5, scales = scales) +
    theme_dementia(xangle = 45) +
    theme(
      strip.background = element_rect(fill = "#DFDDE3"),
      strip.text.x = element_text(size = 9),
      legend.position = "none",
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.spacing = unit(facet_space, "pt"))
  
  ggplotly(plot, tooltip = "text") %>%
    config(
      displayModeBar = TRUE,
      doubleClick = FALSE,
      modeBarButtonsToRemove = list(
        "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", 
        "toggleSpikelines", "hoverCompareCartesian", "hoverClosestCartesian",
        "toImage"),
      displaylogo = FALSE,
      editable = FALSE) %>%
    layout(
      clickmode = "none",
      margin = list(l = -5, b = 50, t = 40))
}
