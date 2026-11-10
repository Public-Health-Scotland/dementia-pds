create_outcomes_table_static <- function(data, type, geog_filter = NULL,  complete_year = T){
  if (type == "wide") {
    data %>% 
      select(
        geog, 
        "Total Referrals" = referrals, 
        "Standard Met" = met, 
        "Standard Not Met" = not_met, 
        "PDS Ongoing" = ongoing, 
        "Exempt" = exempt) %>%
      arrange(geog != "Scotland", geog) %>%
      pivot_longer(cols = -geog, names_to = "ldp", values_to = "vals") %>%
      pivot_wider(values_from = vals, names_from = geog) %>%
      mutate(across(where(is.numeric), ~ format(.x, big.mark = ","))) %>%
      select(ldp, all_of(unique(data$board)), all_of(sort(unique(filter(data, name == "ijb")$geog)))) %>%
      kable(col.names = c("", names(.)[-1]), align = c("l", rep("r", length(names(.)) - 1))) %>%
      kable_styling(full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      add_header_above(c(
        " " = 1,
        "Health Board" = 1,
        "Integration Authority Area" = length(unique(data$geog)) - 1))
    
  } else if (type == "long") {
    data <- data %>% 
      filter(name %in% geog_filter) %>%
      mutate(
        perc_met = paste0(round(perc_met, 1), "%"),
        across(where(is.numeric), ~ format(.x, big.mark = ","))) %>%
      select(
        geog, 
        "Total Referrals" = referrals, 
        "Standard Met" = met, "Standard Not Met" = not_met, 
        "PDS Ongoing" = ongoing, 
        "Exempt" = exempt,
        "% Met Standard/Exempt" = perc_met) %>%
      arrange(geog != "Scotland", geog)
      
    if (complete_year == TRUE){
      data %>%
        kable(
          col.names = c("", names(.)[-1]), 
          align = c("l", rep("r", length(names(.)) - 1))) %>%
        kable_styling(full_width = FALSE) %>%
        column_spec(c(1, 7), bold = T)
    } else {
      data %>%
        select(-"% Met Standard/Exempt") %>%
        kable(
          col.names = c("", names(.)[-1]), 
          align = c("l", rep("r", length(names(.)) - 1))) %>%
        kable_styling(full_width = FALSE) %>%
        column_spec(c(1, 6), bold = T)
    }
  }
}
