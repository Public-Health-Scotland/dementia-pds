################################################################################.
# Name of file - 04_rates-and-outcomes.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Lucy Binsted
# Original Date - August 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - 
#   Join data to create a table of rates and outcomes
################################################################################.

################################################################################.
### 1 - Setup ----
################################################################################.

# Source the setup script
rm(list = ls())
source(here::here("code", "00_setup-environment.R"))
source(here::here("functions/create_rates_table.R"))
source(here::here("functions/create_outcomes_table.R"))

################################################################################.
### 2 - Read data ----
################################################################################.

# PDS data (output from 03_process-data.R)
pds <- read_rds(
  get_mi_data_path(
    type = "ldp_data", 
    ext = "rds", 
    fy = fy, 
    qt = qt,
    test_output = test_output)) 

# European Standard Population lookup (output from 01_update-lookups)
ESP13_df <- read_rds(get_lookup_path(type = "esp"))

# Expected Diagnoses lookup  (output from 01_update-lookups)
exp_df <- read_rds(get_lookup_path(type = "exp"))

# Population lookup (output from 01_update-lookups)
pop_lookup <- read_rds(get_lookup_path(type = "pop"))

# Population lookup (output from 01_update-lookups)
pop_lookup_simd <- read_rds(get_lookup_path(type = "pop_simd"))

################################################################################.
### 3 - Pivot PDS data ----
################################################################################.

pds <- pds %>% 
  mutate(
    # Create a new column called scot (used for calculating Scotland totals)
    scot = "Scotland",
    # Create a new column called board (used for keeping track of which health board each ijb belongs to)
    board = health_board) %>%
  # Put scot, ijb and health board in the same column named geog
  pivot_longer(c(scot, ijb, health_board), values_to = "geog") %>%
  mutate(board = ifelse(name == "scot", "Scotland", board))

################################################################################.
### 4 - Create rates and measures table ----
################################################################################.

# Rates
output <- bind_rows(
  # No dmographic
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df),
    create_outcomes_table(pds, exp_df)) %>%
    mutate(demog = "none"),
  # Age
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df, "age_grp_2"),
    create_outcomes_table(pds, exp_df, "age_grp_2")) %>%
    mutate(demog = "age"),
  # Sex
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df, "sex"),
    create_outcomes_table(pds, exp_df, "sex")) %>%
    mutate(demog = "sex"),
  # SIMD
  full_join(
    create_rates_table(pds, pop_lookup_simd, ESP13_df, "simd"),
    create_outcomes_table(pds, exp_df, "simd")) %>%
    mutate(demog = "simd")) %>%
  mutate(
    # Fill integer missing values with 0
    across(where(is.integer), ~ replace_na(.x, 0)),
    # Remove sex codes
    sex = case_when(
      sex == "01 Male" ~ "Male",
      sex == "02 Female" ~ "Female",
      TRUE ~ sex)
  )

################################################################################.
### 5 - Save data ----
################################################################################.

output %>% 
  write_file(path = get_mi_data_path(
    type = "final_data", 
    ext = "rds", 
    fy = fy,
    qt = qt,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE))

################################ END OF SCRIPT #################################.