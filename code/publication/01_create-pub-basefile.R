################################################################################.
# Name of file - 01_create-pub-basefile.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - March 2021
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Create base file with all data required for publication.
################################################################################.

################################################################################.
### 0 - Load environment file ----
################################################################################.

rm(list = ls())
source(here::here("code", "publication", "00_setup-pub-environment.R"))

################################################################################.
### 1 - Read data ----
################################################################################.

# PDS data (output from 03_process-data.R)
pds <- read_rds(
  get_mi_data_path(
    type = "ldp_data", 
    ext = "rds", 
    fy = fy, 
    qt = qt,
    test_output = test_output)) 

################################################################################.
### 2 - Restructure data file ----
################################################################################.

pds <- pds %>%
  # Select only FY to be included in pub
  filter(fy %in% fy_in_pub) %>%
  rename(ldp_full = ldp) %>%
  mutate(
    # Remove code from gender
    sex = if_else(sex == "Unknown", sex, substring(sex, 4)),
    ldp = word(ldp_full, 1)) %>% 
  # Aggregate to year level (don't need month breakdown)
  group_by(health_board, ijb, fy, ldp, ldp_full, age_grp_2, simd, sex) %>%
  summarise(referrals = n(), .groups = "drop") %>%
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  # Add summary columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>%
  # Exclude Aberdeen City from standard calculation for 2019/20 and 2020/21, while retaining 
  # referral counts. Remove the relevant financial year below if Aberdeen City is reinstated.
  mutate(
    across(
      c(complete, exempt, fail, ongoing, numerator, denominator),
      ~ if_else(
        ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), 0L, .x)))

################################################################################.
### 3 - Save file ----
################################################################################.

pds %>% 
  write_file(
    path = get_pub_data_path(
      pub_date = pub_date, 
      check_mode = "write",
      create_dir = TRUE))
#0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

################################ END OF SCRIPT #################################.