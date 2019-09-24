
set_env <- function(data = pds, 
                    exp = exp,
                    error = error,
                    hb,
                    year,
                    quarter){
  
  subpage_env <- new.env()
  
  # Filter data for selected Health Board and FY
  if(hb == "Scotland"){
    
    subpage_data <-
      data %>%
      filter(fy == year)
    
    subpage_err <-
      err %>%
      filter(fy == year)
    
  }else{
    
    subpage_data <-
      data %>%
      filter(fy == year & health_board == hb)
    
    subpage_err <-
      err %>%
      filter(fy == year)
    
  }
  
  # Get Scotland data for health board benchmarking
  if(hb != "Scotland"){
    subpage_data_scot <-
      data %>%
      filter(fy == year)
  }
  
  # Get expected diagnoses figure for selected Health Board and FY
  subpage_exp  <- exp %>%
    filter(health_board_label == hb & fy == year) %>%
    pull(diagnoses)
  
  subpage_exp_scot <- sum(exp$diagnoses)
  
  # Assign objects to subpage environment
  assign("subpage_data", subpage_data, subpage_env)
  assign("hb", hb, subpage_env)
  assign("sel_fy", year, subpage_env)
  assign("max_fy", max(sort(unique(data$fy))), subpage_env)
  assign("all_fy", setdiff(unique(data$fy), year), subpage_env)
  assign("exp", subpage_exp, subpage_env)
  assign("err", subpage_err, subpage_env)
  assign("qt", quarter, subpage_env)
  
  if(hb != "Scotland"){
    assign("subpage_data_scot", subpage_data_scot, subpage_env)
    assign("exp_scot", subpage_exp_scot, subpage_env)
  }
  
  return(
    if(hb == "Scotland"){
      knitr::knit_child("subpage-scotland.Rmd", envir = subpage_env)
    }else{
      knitr::knit_child("subpage-hb.Rmd", envir = subpage_env)
    }
  )
  
}
