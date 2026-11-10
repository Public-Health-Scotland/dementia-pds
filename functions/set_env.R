set_env <- function(data = pds,
                    monthly_referrals = monthly_referrals,
                    hb,
                    year,
                    quarter){
  
  # Set new environment
  subpage_env <- new.env()
  
  # Filter data for selected Health Board and FY
  if (hb == "Scotland") {
    subpage_data <- data %>% 
      filter(fy == year, demog == "none")
  } else {
    subpage_data <- data %>% 
      filter(fy == year, board == hb, demog == "none")
  }
  
  # Get expected diagnoses figure for selected Health Board and FY
  subpage_monthly_referrals  <- monthly_referrals %>%
    filter(fy == year, board == hb)
  
  # Assign objects to subpage environment
  assign("subpage_data", subpage_data, subpage_env)
  assign("subpage_monthly_referrals", subpage_monthly_referrals, subpage_env)
  assign("hb", hb, subpage_env)
  assign("sel_fy", year, subpage_env)
  assign("max_fy", max(sort(unique(data$fy))), subpage_env)
  assign("min_fy", min(sort(unique(data$fy))), subpage_env)
  assign("all_fy", sort(unique(data$fy)), subpage_env)
  assign("qt", quarter, subpage_env)
  assign("start_date", start_date, subpage_env)

  return(
    if(hb == "Scotland"){
      knitr::knit_child("subpage-scotland.Rmd", envir = subpage_env)
    }else{
      knitr::knit_child("subpage-hb.Rmd", envir = subpage_env)
    }
  )
}

# subpage_data<-get("subpage_data", envir = subpage_env)
# subpage_monthly_referrals<-get("subpage_monthly_referrals", envir = subpage_env)
# hb<-get("hb", envir = subpage_env)
# sel_fy<-get("sel_fy", envir = subpage_env)
# max_fy<-get("max_fy", envir = subpage_env)
# min_fy<-get("min_fy", envir = subpage_env)
# all_fy<-get("all_fy", envir = subpage_env)
# qt<-get("qt", envir = subpage_env)
# start_date<-get("start_date", envir = subpage_env)
