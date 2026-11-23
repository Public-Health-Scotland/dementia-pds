################################################################################.
### Manual Variables - TO UPDATE FOR MI REPORT
################################################################################.

# UPDATE - Financial Year (YYYY) and Quarter (Q) of the submission data
# Each submission contains data for the previous 3 full financial years 
# and the current financial year up to the current quarter.
#  - Sep-MI-release: Q1 data (1 Apr - 30 June)
#  - Dec-MI-release: Q2 data (1 Jul - 30 Sept)
#  - Mar-MI-release: Q3 data (1 Oct - 31 Dec)
#  - Jun-MI-release: Q4 data (1 Jan - 31 March)
fy <- 2026
qt <- 2

# UPDATE - Used to define a test file path for saving test outputs (TRUE/FALSE)
#  - TRUE:  Use this when the Data Management Team provide a test version of the 
#           collated file and boards are still submitting or correcting data.
#  - FALSE: Use this when the Data Management Team provide a finalised version of  
#           the collated file and no more changes will be made.
test_output <- TRUE

# Source setup script
source(here::here("code/00_setup-environment-code.R"))

################################################################################.
# This will run all scripts to prepare data and create MI report
# DO NOT LEAVE UNCOMMENTED
# for (file in list.files("code")[3:7]){
#   source(paste0("code/", file))
# }
################################################################################.
