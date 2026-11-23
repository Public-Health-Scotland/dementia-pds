create_trend_table_static <- function(data, measure, geog_filter) {
  
  geog_name <- case_when(
    "ijb" %in% geog_filter & "health_board" %in% geog_filter ~ "Integration Authority Area/Health Board",
    "ijb" %in% geog_filter ~ "Integration Authority Area",
    "health_board" %in% geog_filter ~ "Health Board"
  )
  
  if (rlang::as_name(rlang::ensym(measure)) == "referrals"){
    data <- data %>%  mutate(referrals = format(referrals, big.mark = ","))
  }
  
  if (rlang::as_name(rlang::ensym(measure)) == "rate"){
    data <- data %>% mutate(rate = round(rate, 1))
  }
  
  if (rlang::as_name(rlang::ensym(measure)) == "exp_rate"){
    data <- data %>% mutate(rate = round(exp_rate, 1))
  }
  
  if (rlang::as_name(rlang::ensym(measure)) == "perc_met"){
    data <- data %>% mutate(perc_met = paste0(round(perc_met, 1), "%"))
  }
  
  static_table <- data %>%
    arrange(desc(name)) %>%
    filter(name %in% geog_filter) %>%
    select(geog, fy, {{ measure }}) %>%
    pivot_wider(names_from = fy, values_from = {{ measure }}) %>%
    rename(!!geog_name := geog) %>%
    kable(align = c("l", rep("r", length(unique(data$fy))))) %>%
    kable_styling(full_width = TRUE, c("striped", "hover")) %>%
    column_spec(1, bold = TRUE) %>%
    add_header_above(c(" " = 1, "Financial Year of Diagnosis" = length(unique(data$fy)))) %>%
    scroll_box(height = "685px")
  
  return(static_table)
}
