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
cross_yr_comparison_hb <- cross_year_measures(latest_data, var = health_board) %>% 
                          write_tests_xlsx(sheet_name = "cross_yr_hb")  
  
# create a cross year comparison - IJB level
cross_yr_comparison_ijb <- cross_year_measures(latest_data, var = ijb) %>% 
                           write_tests_xlsx(sheet_name = "cross_yr_ijb")



### Produce comparison to previous quarter submission --------------------------

hb_comparison <- produce_test_comparison(calculate_measures(previous_data, 
                                                            var = health_board),
                                         calculate_measures(latest_data, 
                                                            var = health_board)) %>% 
                  arrange(fy, measure) %>% 
                  write_tests_xlsx(sheet_name = "HB_comparison")

ijb_comparison <- produce_test_comparison(calculate_measures(previous_data, 
                                                             var = ijb),
                                          calculate_measures(latest_data, 
                                                             var = ijb)) %>% 
                  arrange(fy, measure) %>%  
                  write_tests_xlsx(sheet_name = "IJB_comparison")


# End of Script # 
