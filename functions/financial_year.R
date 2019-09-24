
financial_year <- function(date){
  
  if_else(month(date) >= 4,
          paste0(year(date), "/", substr(year(date) + 1, 3, 4)),
          paste0(year(date) - 1, "/", substr(year(date), 3, 4))
          )
  
}
