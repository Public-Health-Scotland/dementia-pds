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
source(here::here("code", "00_setup-environment.R"))
source(here::here("functions/create_rates_table.R"))
source(here::here("functions/create_outcomes_table.R"))

################################################################################.
### 2 - Read data ----
################################################################################.

# PDS data (output from 03_add-ldp-age-simd.R)
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
### 3 - Create rates and measures table ----
################################################################################.

# Rates
output <- bind_rows(
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df),
    create_outcomes_table(pds, exp_df)) %>%
    mutate(demog = "none"),
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df, "age_grp_2"),
    create_outcomes_table(pds, exp_df, "age_grp_2")) %>%
    mutate(demog = "age"),
  full_join(
    create_rates_table(pds, pop_lookup, ESP13_df, "sex"),
    create_outcomes_table(pds, exp_df, "sex")) %>%
    mutate(demog = "sex"),
  full_join(
    create_rates_table(pds, pop_lookup_simd, ESP13_df, "simd"),
    create_outcomes_table(pds, exp_df, "simd")) %>%
    mutate(demog = "simd"),
)

################################################################################.
### 4 - Save data ----
################################################################################.

pds %>% 
  write_file(path = get_mi_data_path(
    type = "final_data", 
    ext = "rds", 
    fy = fy,
    qt = qt,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE))

################################ END OF SCRIPT #################################.