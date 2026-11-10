plot_monthly_referrals <- function(data){
  
  # Determine whether to show IJB breakdown
  if (length(unique(data$geog)) < 3) {
    data <- data %>% 
      filter(name != "health_board")
  } else {
    data <- data %>% 
      mutate(geog = factor(geog, levels = sort(unique(geog)))) %>%
      mutate(geog = forcats::fct_relevel(geog, max(.$board)))
  }
  
  data <- data %>%
    mutate(
      year = if_else(
        month %in% 1:3,
        paste0(substr(fy, 1, 2), substr(fy, 6, 7)),
        substr(fy, 1, 4)),
      month_full = month(month, label = TRUE, abbr = FALSE),
      month_abbr = forcats::fct_relevel(
        month(month, label = TRUE),
        "Jan", "Feb", "Mar", after = Inf))
  
  plot <- ggplot(data, aes(
    x = month_abbr, 
    y = monthly_referrals, 
    group = geog,
    colour = geog,
    text = paste0(
      geog, 
      "<br>", month_full, " ", year, "<br>", "Referrals: ", 
      format(monthly_referrals, big.mark = ",")))) +
    geom_point() +
    geom_line() + 
    scale_y_continuous(limits = c(0, NA), breaks = integer_breaks()) +
    scale_colour_manual(values = phs_colours_core) +
    scale_x_discrete(
      labels = paste(levels(data$month_abbr), c(rep(min(data$year), 9), rep(max(data$year), 3)))) +
    labs(x = "Month of Diagnosis", y = "Number Referred", colour = NULL) +
    theme_dementia(xangle = 45) +
    theme(legend.title = element_blank(),
          legend.position = ifelse(length(unique(data$geog)) < 3, "none", "bottom"))

   ggplotly(plot, tooltip = "text") %>%
     config(
       displayModeBar = TRUE,
       modeBarButtonsToRemove = list(
         'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'toggleSpikelines', 
         'hoverCompareCartesian', 'hoverClosestCartesian', 'toImage'), 
       displaylogo = F, editable = F) %>%
     layout(legend = list(
       orientation = "h", x = 0.5 , y = -0.6, xanchor = "center", yanchor = "bottom"))
}
