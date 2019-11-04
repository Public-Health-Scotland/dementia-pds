
sub_month <- function(fy, qt){
  
  month <- 
    case_when(
      qt == 1 ~ "July",
      qt == 2 ~ "October",
      qt == 3 ~ "January",
      qt == 4 ~ "April"
    )
  
  year <- 
    case_when(
      qt %in% 1:2 ~ substr(fy, 1, 4),
      qt %in% 3:4 ~ paste0("20", substr(fy, 6, 7))
    )
  
  paste(month, year)
  
}
