################################################################################
# Name of file - 04_ldp_tests.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Jennifer Thom
# Original Date - August 2024
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Produce tests for ldp standard
################################################################################

### 1 - Load environment file and functions-------------------------------------

source(here::here("code", "00_setup-environment.R"))

source(here::here("functions", "produce_tests.R"))


### 2 - Load data files --------------------------------------------------------

# Read latest data---
latest_data <- read_rds(get_mi_data_path(type = "final_data", 
                                         "rds", 
                                         test_output = FALSE)) %>% 
              # Remove codes from board and IJB
              clean_geog_codes()

# read previous years data---
previous_data <- read_rds(get_mi_data_path(type = "final_data", 
                                           "rds", 
                                           previous_data = TRUE)) %>% 
              # Remove codes from board and IJB
              clean_geog_codes()


### Produce cross year tests ---------------------------------------------------

# create a cross year comparison - Health Board level
cross_yr_comparison_hb <- cross_year_measures(latest_data, var = health_board)
  
# create a cross year comparison - IJB level
cross_yr_comparison_ijb <- cross_year_measures(latest_data, var = ijb)


## TODO - write to excel workbook 


### Produce comparison to previous quarter submission --------------------------

hb_comparison <- produce_test_comparison(calculate_measures(previous_data, 
                                                            var = health_board),
                                         calculate_measures(latest_data, 
                                                            var = health_board))

ijb_comparison <- produce_test_comparison(calculate_measures(previous_data, 
                                                             var = ijb),
                                          calculate_measures(latest_data, 
                                                             var = ijb))

total_comparison <- bind_rows(hb_comparison, ijb_comparison) %>% 
                    arrange(fy, measure)

## TODO - write to excel workbook 


# End of Script # 
