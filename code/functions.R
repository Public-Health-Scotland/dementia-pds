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


## TO DO - add more comments


## Add Financial Year Variable in format 17/18

finyear <- function(date, start_month = 4){
  
  if_else(month(date) >= start_month,
          paste0(substr(year(date), 3, 4), "/", substr(year(date) + 1, 3, 4)),
          paste0(substr(year(date) - 1, 3, 4), "/", substr(year(date), 3, 4))
  )
  
}


## Add certain number of months to a given date
## Using %m+% operator rolls back date e.g. 31/05/2019 + 1 month = 30/06/2019
## This function defines this as 01/07/2019

add_months <- function(date, no_months){
  
  if_else(is.na(date + months(no_months)),
          date %m+% months(no_months) + days(1),
          date + months(no_months))
  
}