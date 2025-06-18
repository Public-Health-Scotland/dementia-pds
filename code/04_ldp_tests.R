################################################################################
# Name of file - 04_ldp_tests.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Jennifer Thom
# Original Date - August 2024
# Update by Abram McCormick - Jan 2025
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Produce tests for ldp standard
################################################################################

### 1 - Load environment file and functions-------------------------------------

source(here::here("code", "00_setup-environment.R"))

walk(dir(here::here("functions"), full.names = TRUE), source)

### 2 - produce comparison to previous year to current quarter ------------


# Read latest data---
latest_data <- read_rds(get_mi_data_path(type = "final_data", 
                                         "rds", 
                                         test_output = test_output)) %>% 
  # Remove codes from board and IJB
  clean_geog_codes()

#get current year in YYYY/YY format
current_fy <- paste0(substr(fy,1,4),
                     "/", as.numeric(substr(fy,3,4)) + 1)

# calculate totals for current year
pds_current_yr_summary <- latest_data %>% filter(fy == current_fy) %>% 
  group_by(ijb, fy) %>% summarise(total_referrals = sum(referrals)) %>% select(-fy)

colnames(pds_current_yr_summary) <- c("ijb",paste0(current_fy, " at Q", qt))


# Load PDS data for previous year up to current quarter
pds_previous_year_to_qt <- 
  read_rds(get_mi_data_path("final_data",
                            ext = "rds",
                            test_output = FALSE,
                            previous_year_to_qt = TRUE)) %>% 
  # Remove codes from board and IJB
  clean_geog_codes()

# write previous year in YYYY/YY format
previous_fy_full <- paste0(substr(as.numeric(fy)-1,1,4),
                           "/", as.numeric(substr(fy,3,4)))

# calculate totals for previous year up to current quarter
pds_previous_yr_to_qt_summary <- pds_previous_year_to_qt %>% filter(fy == previous_fy_full) %>% 
  group_by(ijb, fy) %>% summarise(total_referrals = sum(referrals)) %>% select(-fy)

colnames(pds_previous_yr_to_qt_summary ) <- c("ijb",paste0(previous_fy_full, " at Q", qt))

# join data and calculate Scotland totals
quarter_comparison <- left_join(pds_current_yr_summary,pds_previous_yr_to_qt_summary) %>% 
  adorn_totals(name = "Scotland") %>% 
# calculate percentage of last year up to current quarter
  rowwise() %>% 
  mutate(perc_of_prev_qt = paste0(round(sum(c_across(2))/sum(c_across(3)),3))) %>%   
  mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  rename("Integration Authority Area" = "ijb")

names(quarter_comparison)[ncol(quarter_comparison)] <- 
  paste0(current_fy, " at Q", qt, " as % of ", previous_fy_full, " at Q", qt)

quarter_comparison %>% 
  write_tests_xlsx(sheet_name = "IJB_qt_on_qt")


### 3 - Produce cross year tests --------------------------------------------------------


# read previous years data---
previous_data <- read_rds(get_mi_data_path(type = "final_data", 
                                           "rds", 
                                           test_output = FALSE,
                                           previous_data = TRUE)) %>% 
              # Remove codes from board and IJB
              clean_geog_codes()


# create a cross year comparison - Health Board level
cross_yr_comparison_hb <- cross_year_measures(latest_data, var = health_board) %>% 
                          write_tests_xlsx(sheet_name = "cross_yr_hb")  
  
# create a cross year comparison - IJB level
cross_yr_comparison_ijb <- cross_year_measures(latest_data, var = ijb) %>% 
                           write_tests_xlsx(sheet_name = "cross_yr_ijb")



###  4 - Produce comparison to previous quarter submission --------------------------

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
