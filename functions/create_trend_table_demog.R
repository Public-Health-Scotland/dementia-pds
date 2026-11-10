create_trend_table_demog <- function(data, measure, demog, group){
  
  page_length <- length(unique(data[[demog]]))
  page_length <- ifelse(measure == "outcome", page_length*2, page_length)
  
  demog_name <- case_when(
    demog == "age_grp_2" ~ "Age Group",
    demog == "sex" ~ "Sex",
    demog == "simd" ~ "Deprivation Quintile")
  
  if (measure == "rate"){
    data <- data %>% 
      mutate(rate = ifelse(!is.na(rate), round(rate, 1), "-")) %>%
      pivot_wider(names_from = fy, values_from = {{ measure }}) %>%
      arrange(demog) %>% 
      rename(!!demog_name := demog) %>%
      select(-name)
    
  } else if (measure == "outcome"){
    data <- data %>% 
      mutate(
        perc_met = ifelse(!is.na(perc_met), paste0(perc_met, "%"), "-"), 
        refs = ifelse(!is.na(refs), format(refs, big.mark = ","), "-")) %>% 
      pivot_longer(cols = c("refs", "perc_met"), names_to = " ", values_to = "outcome") %>%
      mutate(` ` = ifelse(` ` == "refs", "Number of Referrals (excludes PDS Ongoing)", "% Met Standard/Exempt")) %>%
      pivot_wider(names_from = fy, values_from = {{ measure }}) %>%
      arrange(demog) %>% 
      mutate({{ demog }} := ifelse(` ` == "% Met Standard/Exempt", "", .data[[demog]])) %>%
      rename(!!demog_name := demog) %>%
      select(-name)
  }
  
  table_data <- SharedData$new(
    data,
    key = ~geog, 
    group = group)
  
  bscols(
    widths = 12,
    div(
      style = css(width="97%", height="165px", margin="20px"),
      DT::datatable(
        table_data, 
        rownames = FALSE, 
        selection = 'none',
        options = list(
          pageLength = page_length,
          scrollX = TRUE,
          scrollY = FALSE,
          dom = "t",
          ordering = FALSE,
          columnDefs = list(list(visible = FALSE, targets = 0)))) %>%
        DT::formatStyle(2, fontWeight = "bold")))
}
  