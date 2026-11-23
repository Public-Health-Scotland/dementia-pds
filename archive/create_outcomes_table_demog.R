create_outcomes_table_demog <- function(data, measure, demog, geog_filter, dropdown_grp) {
  
  geog_name <- case_when(
    "ijb" %in% geog_filter & "health_board" %in% geog_filter ~ "Integration Authority Area/Health Board",
    "ijb" %in% geog_filter ~ "Integration Authority Area",
    "health_board" %in% geog_filter ~ "Health Board")
  
  if (measure == "rate"){
    data <- data %>% 
      mutate(rate = ifelse(!is.na(rate), round(rate, 1), "-")) %>%
      filter(name %in% geog_filter) %>%
      pivot_wider(names_from = {{ demog }}, values_from = {{ measure }}) %>%
      mutate(key = paste0(fy, geog)) %>%
      arrange(desc(name)) %>%
      rename(!!geog_name := geog)
    
  } else if (measure == "outcome"){
    data <- data %>% 
      mutate(
        perc_met = ifelse(!is.na(perc_met), paste0(perc_met, "%"), "-"), 
        refs = ifelse(!is.na(refs), format(refs, big.mark = ","), "-")) %>% 
      pivot_longer(cols = c("refs", "perc_met"), names_to = " ", values_to = "outcome") %>%
      mutate(` ` = ifelse(` ` == "refs", "Number of Referrals (excludes PDS Ongoing)", "% Met Standard/Exempt")) %>%
      filter(name %in% geog_filter) %>%
      pivot_wider(names_from = {{ demog }}, values_from = {{ measure }}) %>%
      mutate(key = paste0(fy, geog)) %>%
      arrange(desc(name)) %>%
      mutate(geog = ifelse(` ` == "% Met Standard/Exempt", "", as.character(geog))) %>%
      rename(!!geog_name := geog)
  }
  
  table_data <- SharedData$new(
    data,
    key = ~key,
    group = dropdown_grp)
  
  rate_targets <- which(names(table_data$origData()) %in% unique(data[[demog]])) - 1
  
  bscols(
    widths = 12, 
    div(
      style = css(width="97%", height="560px", margin="20px"),
      DT::datatable(
        table_data, 
        rownames = FALSE, 
        selection = 'none',
        options = list(
          pageLength = nrow(table_data$origData()),
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = c("fy", "key", "name")),
            list(className = 'dt-right', targets =  as.list(rate_targets))))) %>%
        DT::formatStyle(
          columns = intersect(c(geog_name, " "), names(data)),
          fontWeight = "bold") %>%
        DT::formatStyle(
          columns = "name",
          target = "row",
          fontWeight = DT::styleEqual("scot", "bold"))))
}
