################################################################################.
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
################################################################################.

################################################################################.
### 1 - Load environment file, functions and data ----
################################################################################.

source(here::here("code", "00_setup-environment.R"))
walk(dir(here::here("functions"), full.names = TRUE), source)

# Current data (grouped)
final_data_curr <- read_rds(get_mi_data_path(
  type = "final_data", 
  ext = "rds", 
  fy = fy,
  qt = qt,
  test_output = test_output))

# Current LDP data (individual)
ldp <- read_rds(get_mi_data_path(
  type = "ldp_data", 
  ext = "rds", 
  fy = fy,
  qt = qt,
  test_output = test_output)) 

# Previous years data (up to current quarter)
final_data_prev_yr <- read_rds(get_mi_data_path(
  type = "final_data", 
  ext = "rds", 
  fy = as.character(as.numeric(fy)-1),
  qt = qt,
  test_output = FALSE))

# Previous quarters data
final_data_prev_qt <- read_rds(get_mi_data_path(
  type = "final_data", 
  ext = "rds", 
  fy = previous_fy,
  qt = previous_qt,
  test_output = FALSE))

# Current year in YYYY/YY format
current_fy <- paste0(
  substr(fy,1,4), "/", 
  as.numeric(substr(fy,3,4)) + 1
)

# Previous year in YYYY/YY format
previous_fy_full <- paste0(
  substr(as.numeric(fy)-1,1,4), "/", 
  as.numeric(substr(fy,3,4))
)

################################################################################.
### 2 - Produce comparison to previous year to current quarter ----
################################################################################.

yr_qt_comparison <- left_join(
  # Calculate total referrals for current year
  final_data_curr %>% 
    filter(fy == current_fy) %>% 
    group_by(geog, name) %>% 
    summarise(total_referrals_curr = sum(referrals, na.rm = TRUE)),
  # Calculate total referrals for previous year up to current quarter
  final_data_prev_yr %>% 
    filter(fy == previous_fy_full) %>% 
    group_by(geog, name) %>% 
    summarise(total_referrals_prev = sum(referrals, na.rm = TRUE))
) %>%
  # Calculate percentage
  mutate(perc = paste0(round(total_referrals_curr/total_referrals_prev, 3) * 100, "%")) %>%
  mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>% 
  arrange(desc(name)) %>%
  select(
    "IAA/HB" = geog,
    !!paste0(current_fy, " at Q", qt) := total_referrals_curr,
    !!paste0(previous_fy_full, " at Q", qt) := total_referrals_prev,
    !!paste0(current_fy, " at Q", qt, " as % of ", previous_fy_full, " at Q", qt) := perc
  )

################################################################################.
### 3 - Produce cross year tests ----
################################################################################.

yr_comparison <- final_data_curr %>%
  group_by(geog, name, fy) %>%
  summarise(referrals = sum(referrals, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(name)) %>%
  pivot_wider(
    id_cols = geog,
    names_from = fy,
    values_from = referrals
  ) %>%
  rename("IAA/HB" = geog)

################################################################################.
###  4 - Produce comparison to previous quarter submission ----
################################################################################.

qt_comparison <- produce_test_comparison(
  final_data_prev_qt %>%
    group_by(geog, name, fy) %>%
    summarise(referrals = sum(referrals, na.rm = TRUE), .groups = "drop") %>%
    arrange(fy, desc(name)) %>%
    select("IAA/HB" = geog, fy, referrals),
  final_data_curr %>%
    group_by(geog, name, fy) %>%
    summarise(referrals = sum(referrals, na.rm = TRUE), .groups = "drop") %>%
    arrange(fy, desc(name)) %>%
    select("IAA/HB" = geog, fy, referrals)
)

################################################################################.
### 5 Analysing those with contact before diagnosis and ldp classification complete in latest submission ----
################################################################################.

# Filter for those with a contact date before diagnosis that are marked as meeting the standard
final <- ldp %>% 
  filter(
    date_of_initial_first_contact < dementia_diagnosis_confirmed_date, 
    !fy %in% finalised_years, grepl("complete", ldp)
  ) %>%
  mutate(
    # Date 11 months after diagnosis date
    diag_11 = add_with_rollback(
      dementia_diagnosis_confirmed_date, 
      months(11),
      roll_to_first = TRUE
    ),
    # Date 12 months after diagnosis date
    diag_12 = add_with_rollback(
      dementia_diagnosis_confirmed_date, 
      months(12), 
      roll_to_first = TRUE),
    
    # Add ldp classification replacing date of first contact with diagnosis date
    ldp_if_diag_date_used_as_contact_date = case_when(
      
      # COMPLETE
      # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
      date_of_initial_first_contact < diag_12 & 
        end_date >= diag_12 &
        is.na(termination_or_transition_date)
      ~ "complete - Without termination date",
      # Started PDS within 12m of diagnosis AND PDS ended after 11m
      date_of_initial_first_contact < diag_12 &
        termination_or_transition_date >= diag_11
      ~ "complete - PDS ended",
      
      # FAIL
      # PDS started more than 12m after diagnosis
      date_of_initial_first_contact >= diag_12
      ~ "fail - PDS started more than 12 months after diagnosis",
      # More than 12m since diagnosis and PDS not started
      end_date >= diag_12 & 
        is.na(date_of_initial_first_contact) &
        is.na(termination_or_transition_date)
      ~ "fail - PDS not started and more than 12 months since diagnosis",
      # PDS terminated before 11 months from start date
      termination_or_transition_date < diag_11 &
        !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
      ~ "fail - PDS terminated less than 11 months after first contact",
      # PDS terminated before first contact made
      is.na(date_of_initial_first_contact) & 
        !is.na(termination_or_transition_date) & 
        !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
      ~ "fail - PDS terminated before first contact",
      
      # EXEMPT
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
      
      # ONGOING
      # Less than 12m since diagnosis and PDS not started
      end_date < diag_12 & 
        is.na(date_of_initial_first_contact) & 
        is.na(termination_or_transition_date)
      ~ "ongoing - PDS not started and less than 12 months since diagnosis",
      # PDS started within 12m of diagnosis but not yet ended
      date_of_initial_first_contact < diag_12 &
        end_date < diag_12 &
        is.na(termination_or_transition_date)
      ~ "ongoing - Still receiving PDS and less than 12 months since first contact")
  ) %>%
  mutate(
    # Adds flag if ldp classification is unchanged when using diagnosis date as date of first contact
    same_ldp = if_else(ldp == ldp_if_diag_date_used_as_contact_date, 1, 0),
    # Adds flag if date of first contact is on or after date of referral
    contact_on_or_after_referral = case_when(
      date_of_initial_first_contact >= date_pds_referral_received ~ 1,
      TRUE ~ 0),
    # Adds flag if date of first contact is on or after date of allocation
    contact_on_or_after_allocation = case_when(
      date_of_initial_first_contact >= initial_pds_practitioner_allocation_date ~1,
      TRUE ~ 0)
  ) %>%
  filter(name == "ijb") %>%
  select(
    fy, dementia_diagnosis_confirmed_date, date_pds_referral_received,
    initial_pds_practitioner_allocation_date, date_of_initial_first_contact, 
    termination_or_transition_date, termination_or_transition_reason, geog, board,
    ldp, ldp_if_diag_date_used_as_contact_date, 
    same_ldp, contact_on_or_after_referral, contact_on_or_after_allocation
  )

################################################################################.
### 6 Write tests to workbook ----
################################################################################.

yr_qt_comparison %>% 
  write_tests_xlsx(sheet_name = "IJB_qt_on_qt")

yr_comparison %>% 
  write_tests_xlsx(sheet_name = "cross_yr")  

qt_comparison %>% 
  write_tests_xlsx(sheet_name = "prev_qt_comparison")

final %>% 
  write_tests_xlsx(sheet_name = "contact_b4_diag_met_stan")

################################ END OF SCRIPT #################################.