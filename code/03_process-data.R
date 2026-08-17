################################################################################.
# Name of file - 03_process-data.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
# Updated by - Jennifer Thom
# Date - November 2023
# Updated by Abram McCormick - January 2025
# Updated by - Lucy Binsted
# Date - August 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - 
#   Add LDP Standard classification, age groups and SIMD to PDS data
#   Pivot data long to create a geography column (rather than seperate Health Board and IJB columns)
#   Select columns
################################################################################.

################################################################################.
### 1 - Setup ----
################################################################################.

# Environment file
source(here::here("code", "00_setup-environment.R"))

################################################################################.
### 2 - Read data ----
################################################################################.

# Cleaned PDS data (output from 02_data-preparation.R)
pds <- read_rds(
  get_mi_data_path(
    type = "clean_data", 
    ext = "rds", 
    fy = fy,
    qt = qt,
    test_output = test_output))

# SIMD data (output from 01_update-lookups.R)
simd_lookup <- read_rds(
  get_lookup_path(type = "simd"))

# Aberdeen City data for 2019/20 and 2020/21
pds_aberdeen <- bind_rows(
  read_rds(get_ac_data_path("2019")),
  read_rds(get_ac_data_path("2020"))
)

################################################################################.
### 3 - Add LDP Classification ----
################################################################################.

pds <- pds %>% mutate(
  # Financial year
  fy = extract_fin_year(dementia_diagnosis_confirmed_date),
  # Month
  month = month(dementia_diagnosis_confirmed_date),
  # Date 12 months after diagnosis date
  diag_12 = add_with_rollback(dementia_diagnosis_confirmed_date, months(12), roll_to_first = TRUE),
  # Date 11 months after date of first PDS contact     
  pds_11 = add_with_rollback(date_of_initial_first_contact, months(11), roll_to_first = TRUE),
  # Date 12 months after date of first PDS contact
  pds_12 = add_with_rollback(date_of_initial_first_contact, months(12), roll_to_first = TRUE),
  
  # LDP classification
  ldp = case_when(
    
    # COMPLETE
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    date_of_initial_first_contact < diag_12 & 
      end_date >= pds_12 &
      is.na(termination_or_transition_date)
    ~ "complete - Without termination date",
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    date_of_initial_first_contact < diag_12 &
      termination_or_transition_date >= pds_11
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
    termination_or_transition_date < pds_11 &
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
      end_date < pds_12 &
      is.na(termination_or_transition_date)
    ~ "ongoing - Still receiving PDS and less than 12 months since first contact")
)

################################################################################.
### 4 - Add SIMD Data ----
################################################################################.

pds <- bind_rows(
  
  # Add SIMD data
  pds %>%
    mutate(postcode = format_postcode(postcode)) %>%
    left_join(simd_lookup, by = c("postcode" = "pc7")) %>%
    mutate(simd = replace_na(simd, "Unknown")), 
  
  # Update SIMD for Aberdeen City 2019/20 and 2020/21
  pds_aberdeen %>%
    select(-simd) %>% 
    mutate(postcode = format_postcode(postcode)) %>%
    left_join(simd_lookup, by = c("postcode" = "pc7")) %>%
    mutate(simd = replace_na(simd, "Unknown")),
  ) %>%
  
  # Remove 2019/20 and 2020/21 Aberdeen City records if they are duplicated in the latest submission
  group_by(chi_number) %>%
  mutate(dupe = if_else(!is.na(chi_number) & n() > 1, 1, 0)) %>%
  ungroup() %>%
  filter(dupe == 1 & ldp != "Aberdeen City Exemption" | dupe != 1)

################################################################################.
### 5 - Add Age Groups ----
################################################################################.

pds <- pds %>% mutate(
  # Age
  age = floor(time_length(
    interval(date_of_birth, dementia_diagnosis_confirmed_date), 
    "years")),
  # Age groups
  age_grp = case_when(
    age <= 0 | is.na(age) ~ "Unknown",
    age < 90 ~ paste0((age %/% 5) * 5, "-", (age %/% 5) * 5 + 4, " years"),
    age >= 90 ~ "90plus years"),
  # Broad age groups
  age_grp_2 = case_when(
    age <= 0 | is.na(age) ~ "Unknown",
    age %in% 1:79 ~ "79 and Under",
    age %in% 80:84 ~ "80 to 84",
    age >= 85     ~ "85+")
)

################################################################################.
### 6 - Process Data ----
################################################################################.

pds <- pds %>%
  mutate(
    # Flag if contact date is before diagnosis date
    contact_before_diag = case_when(
      date_of_initial_first_contact < dementia_diagnosis_confirmed_date ~1,
      TRUE ~0),
    # Remove health board and ijb codes
    ijb = str_sub(ijb, 11, -1),
    health_board = str_sub(health_board, 3, -1), 
    # Create a new column called scot (used for calculating Scotland totals)
    scot = "Scotland",
    # Create a new column called board (used for keeping track of which health board each ijb belongs to)
    board = health_board) %>%
  # Put scot, ijb and health board in the same column named geog
  pivot_longer(c(scot, ijb, health_board), values_to = "geog") %>%
  mutate(board = ifelse(name == "scot", "Scotland", board)) %>%
  # Select columns to keep
  select("fy",
         "geog",
         "name",
         "board",
         "sex",
         "age",
         "age_grp",
         "age_grp_2",
         "simd",
         "ldp",
         "dementia_diagnosis_confirmed_date",
         "date_pds_referral_received",
         "initial_pds_practitioner_allocation_date",
         "date_of_initial_first_contact",
         "termination_or_transition_date",
         "termination_or_transition_reason",
         "pds_uptake_decision"
  )

################################################################################.
### 7 - Save data ----
################################################################################.

pds %>% 
  write_file(path = get_mi_data_path(
    type = "ldp_data", 
    ext = "rds", 
    fy = fy,
    qt = qt,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE))

################################ END OF SCRIPT #################################.