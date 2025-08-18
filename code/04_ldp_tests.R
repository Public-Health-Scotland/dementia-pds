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




### 5 analysing those with contact before diagnosis and ldp classification complete in latest submission----

# read in latest individual data file
ldp <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) %>% 
  
  # Remove codes from board, IJB, and sex
  mutate(n_referrals = 1,
         health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

# filter for those with a contact date before diagnosis that are marked as meeting the standard
contact_before_diagnosis_ldp_complete <- ldp %>% filter(contact_before_diag == 1, !fy %in% finalised_years, grepl("complete", ldp))

# adds ldp classification IF diagnosis date was used as date of first contact
contact_before_diagnosis_ldp_complete %<>% 
  
  mutate(
    
    # Date 11 months after diagnosis date
    diag_11 = add_with_rollback(dementia_diagnosis_confirmed_date, 
                                months(11),
                                roll_to_first = TRUE)) %>% 
  
  mutate(ldp_if_diag_date_used_as_contact_date = case_when(
    
    #### COMPLETE ####
    
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    date_of_initial_first_contact < diag_12 & 
      end_date >= diag_12 & # pds_12 changed to diag_12
      is.na(termination_or_transition_date)
    ~ "complete - Without termination date",
    
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    date_of_initial_first_contact < diag_12 &
      termination_or_transition_date >= diag_11 # pds_11 changed to diag_11
    ~ "complete - PDS ended",
    
    # PDS terminated before 11 months from start date
    termination_or_transition_date < diag_11 & # pds_11 changed to diag_11
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail - PDS terminated less than 11 months after first contact",
    
    #### EXEMPT
    
    # Exempt termination reason; died
    substr(termination_or_transition_reason, 1, 2) == "03"
    ~ "exempt - 03 Service user has died",
    
    # Exempt termination reason; moved to other HB
    substr(termination_or_transition_reason, 1, 2) == "04"
    ~ "exempt - 04 Service user has moved to a different Health Board area",
    
    # Exempt termination reason; refused
    substr(termination_or_transition_reason, 1, 2) == "05"
    ~ "exempt - 05 Service user has terminated PDS early/refused",
    
    # Exempt termination reason; can't engage
    substr(termination_or_transition_reason, 1, 2) == "06"
    ~ "exempt - 06 Service user no longer able to engage in PDS",
    
    #### ONGOING 
    
    # PDS started within 12m of diagnosis but not yet ended
    date_of_initial_first_contact < diag_12 &
      end_date < diag_12 & # pds_12 changed to diag_12
      is.na(termination_or_transition_date)
    ~ "ongoing - Still receiving PDS and less than 12 months since first contact",
    
   
  TRUE ~ ldp
    
  ))

# adds flag if ldp classification is unchanged when using diagnosis date as date of first contact
final <- contact_before_diagnosis_ldp_complete %>% 
  mutate(same_ldp = if_else(ldp == ldp_if_diag_date_used_as_contact_date, 1, 0)) %>% 
# adds flag if date of first contact is on or after date of referral
  mutate(contact_on_or_after_referral = case_when(
    date_of_initial_first_contact >= date_pds_referral_received ~1,
    TRUE ~0)
  ) %>% 
# adds flag if date of first contact is on or after date of allocation
  mutate(contact_on_or_after_allocation = case_when(
    date_of_initial_first_contact >= initial_pds_practitioner_allocation_date ~1,
    TRUE ~0)
  ) %>% 
  select(fy, dementia_diagnosis_confirmed_date, date_pds_referral_received,
         initial_pds_practitioner_allocation_date, date_of_initial_first_contact, 
         termination_or_transition_date, 
         termination_or_transition_reason, health_board, ijb, error_flag,
         ldp, ldp_if_diag_date_used_as_contact_date, same_ldp, contact_on_or_after_referral, contact_on_or_after_allocation)


final %>% write_tests_xlsx(sheet_name = "diag before contact met")

# End of Script # 
