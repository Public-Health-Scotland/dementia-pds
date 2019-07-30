#########################################################################
# Name of file - functions.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Defines functions used in other scripts.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Financial Year in format 2017/18 ----

finyear <- function(date, fy_start = 4){
  
  if_else(month(date) >= fy_start,
          glue("{year(date)}/{substr(year(date) + 1, 3, 4)}"),
          glue("{year(date) - 1}/{substr(year(date), 3, 4)}")
  )
  
}


### END OF SCRIPT ###