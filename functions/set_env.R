set_env <- function(data = pds, 
                    exp = exp,
                    hb,
                    year){
  
  subpage_env <- new.env()
  
  # Filter data for selected Health Board and FY
  if(hb == "Scotland"){
    subpage_data <-
      data %>%
      filter(fy == year)
  }else{
    subpage_data <-
      data %>%
      filter(fy == year & health_board == hb)
  }
  
  # Get expected diagnoses figure for selected Health Board and FY
  subpage_exp  <- exp %>%
    filter(health_board_label == hb & fy == year) %>%
    pull(diagnoses)
  
  # Assign objects to subpage environment
  assign("subpage_data", subpage_data, subpage_env)
  assign("hb", hb, subpage_env)
  assign("sel_fy", year, subpage_env)
  assign("max_fy", max(sort(unique(data$fy))), subpage_env)
  assign("all_fy", setdiff(unique(data$fy), year), subpage_env)
  assign("exp", subpage_exp, subpage_env)
  
  return(
    if(hb == "Scotland"){
      knitr::knit_child("subpage-scotland.Rmd", envir = subpage_env)
    }else{
      knitr::knit_child("subpage-hb.Rmd", envir = subpage_env)
    }
  )
  
}
