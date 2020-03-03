
ldp_12_months <- function(pds_data, ..., format = TRUE){
  
  pds_data %<>%
    pivot_wider(names_from = ldp,
                values_from = referrals,
                values_fill = list(referrals = 0)) %>%
    mutate(num = complete + exempt,
           den = complete + exempt + fail) %>%
    group_by(...) %>%
    summarise(perc = sum(num) / sum(den) * 100) %>%
    ungroup()
  
  if(format == TRUE){
    pds_data %>%
      mutate(perc = paste0(sprintf("%.1f", round_half_up(perc, 1)), "%"))
  }else{
    pds_data
  }     
  
}
