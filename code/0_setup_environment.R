
#########################################################################
# Name of file - setup_environment.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Sets up environment required for running quarterly management
# reports. This is the only file which should require updating everytime 
# the process is run.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Load packages ----
library(dplyr)         # For data manipulation in the "tidy" way
library(readr)         # For reading in csv files
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidylog)       # For printing results of some dplyr functions
library(tidyr)         # For data manipulation in the "tidy" way
library(stringr)       # For string manipulation and matching
library(here)          # For the here() function
library(glue)          # For working with strings
library(ggplot2)       # For plotting
library(flexdashboard) # For creating markdown outputs


### 2 - Define Whether Running on Server or Locally ----

# Comment out as appropriate
# platform <- c("server")
platform <- c("locally")


# Define root directory for stats server based on whether script is running 
# locally or on server
filepath <- dplyr::if_else(platform == "server",
                           "/conf/",
                           "//stats/")


### 3 - Extract dates ----

# Define the dates that the data are extracted from and to
# Dates are in ddmmyyyy format

# Start date of reporting period
start_date <- lubridate::dmy(01042016)

# End date of reporting period
end_date   <- lubridate::dmy(31032019)

# FY and Quarter of reporting period
fy         <- "2018"
qt         <- "4"       



### END OF SCRIPT ###