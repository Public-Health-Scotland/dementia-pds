################################################################################.
# Name of file - 04_add-rates-and-measures.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - 
# Original Date - 
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - 
# 
# 
################################################################################.

################################################################################.
### 1 - Load data ----
################################################################################.

# Source the setup script
source(here::here("code", "00_setup-environment.R"))

# Cleaned population data (output from 02_create-population-lookup)
population_df <- read_rds("//conf/dementia/A&I/Analysts/Lucy/Age_Standardisation/population_data.rds")

# PDS data with LDP (output from 03_add-ldp-age-simd.R)
pds <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) 

# European Standard Population data
ESP13_df <- read_csv("/conf/dementia/A&I/Analysts/Lucy/Age_Standardisation/european_standard_population_by_sex.csv")

# Expected Diagnoses data
exp_df <- read_csv(get_exp_diagnoses_path())

################################################################################.
### 2. Prepare Data ----
################################################################################.

# European Standard Population data
ESP13_df <- ESP13_df %>%
  # Convert column names to snake case
  clean_names() %>%
  # Remove code
  mutate(
    sex = case_when(
      sex == "Male" ~ "01 Male",
      sex == "Female" ~ "02 Female"))

# Age groups (created from ESP data)
age_groups <- ESP13_df %>%
  # Extract ages in each group and calculate minimum and maximum
  mutate(
    nums = str_extract_all(age_group, "\\d+"),
    min = as.numeric(sapply(nums, `[`, 1)),
    max = as.numeric(sapply(nums, `[`, 2)),
    max = ifelse(is.na(max), Inf, max)) %>%
  # Select columns
  select(age_group, min, max) %>%
  # Keep only unique rows
  distinct()

# LDP data
pds <- pds %>%
  mutate(
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
  # Add age groups from ESP13
  left_join(age_groups, by = join_by(age >= min, age <= max)) %>% 
  select(-min, -max)

# Population data
population_df <- population_df %>%
  # Add age groups from ESP13
  left_join(age_groups, by = join_by(age >= min, age <= max)) %>% 
  select(-min, -max)

# Expected Diagnoses data
exp_df <- exp_df %>%
  # Rename health board column to geog to match PDS and population data
  rename(geog = health_board_label) %>% 
  # Remove health board code column
  select(-health_board) 

################################################################################.
### 3. Calculate Standardised Rates ----
################################################################################.

# Function to create a table showing referral rates per 10000 population
create_rates_table <- function(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c()){
  
  # 1. Prepare LDP data
  ldp <- pds %>% 
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Remove unknown age/sex/simd if they are the demographic
    { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
    { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
    { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_group), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, age_group, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total referrals
    summarise(
      # Number of referrals
      all_referrals = n(), 
      # Number of referrals above the age cutoff
      age_cutoff_referrals = sum(age >= age_cutoff, na.rm = TRUE),
      # Number of referrals with known age (age standardised) or with known age and sex (age-sex standardised)
      filtered_referrals = sum(!is.na(age_group) & !sex_dummy %in% c("98 Not Specified", "99 Not Known")),
      # Number of referrals above the age cutoff (age standardised) or above the age cutoff with known sex (age-sex standardised)
      age_cutoff_filtered_referrals = sum(age >= age_cutoff & !sex_dummy %in% c("98 Not Specified", "99 Not Known"), na.rm = TRUE),
      .groups = "drop")
  
  # 2. Prepare population data
  population <- population_df %>% 
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_group), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, age_group, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total population
    summarise(
      # Population
      all_population = sum(population_estimate, na.rm = TRUE),
      # Population above the age cutoff
      age_cutoff_population = sum(population_estimate[age >= age_cutoff], na.rm = TRUE),
      .groups = "drop")
  
  # 3. Prepare ESP13 data
  ESP13 <- ESP13_df %>%
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Remove original sex column and duplicate rows (necessary for age standardisation)
    select(-sex) %>% distinct() %>%
    # Create a column that only contains ESP for age groups above the age cutoff
    mutate(age_cutoff_esp = ifelse(as.numeric(str_extract(age_group, "^\\d+")) >= age_cutoff, european_standard_population, 0))
  
  # 4. Join data and calculate rates
  rates_df <- full_join(ldp, population, by = c("fy", "geog", "name", "age_group", "sex_dummy", demographic)) %>%
    left_join(ESP13, by = c("age_group", "sex_dummy")) %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, across(any_of(demographic))) %>%
    summarise(
      
      total_referrals                     = sum(all_referrals, na.rm = TRUE),
      total_filtered_referrals            = sum(filtered_referrals, na.rm = TRUE),
      total_age_cutoff_referrals          = sum(age_cutoff_referrals, na.rm = TRUE),
      total_age_cutoff_filtered_referrals = sum(age_cutoff_filtered_referrals, na.rm = TRUE),
      
      total_population                    = sum(all_population, na.rm = TRUE),
      total_age_cutoff_population         = sum(age_cutoff_population, na.rm = TRUE),
      
      crude_rate                          = total_referrals / total_population * 10000,
      crude_rate_filtered                 = total_filtered_referrals / total_population * 10000,
      crude_rate_age_cutoff               = total_age_cutoff_referrals / total_age_cutoff_population * 10000,
      crude_rate_age_cutoff_filtered      = total_age_cutoff_filtered_referrals / total_age_cutoff_population * 10000,
      
      standardised_rate                   = sum(filtered_referrals / all_population * european_standard_population, na.rm = TRUE) * 10000 / sum(european_standard_population, na.rm = TRUE),
      standardised_rate_age_cutoff        = sum(age_cutoff_filtered_referrals / age_cutoff_population * age_cutoff_esp, na.rm = TRUE) * 10000 / sum(age_cutoff_esp, na.rm = TRUE),
      
      old_rate                            = total_referrals / total_age_cutoff_population * 10000,
      .groups = "drop") %>%
    
    { if (standardisation == "age-sex") rename_with(., ~ paste0(.x, "_AS"), matches("rate|filtered")) else . } %>%
    { if (standardisation == "age") rename_with(., ~ paste0(.x, "_A"), matches("rate|filtered")) else . }
  
  return(rates_df)
}

# Age standardised rates
age_standardised_rates <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age", age_cutoff = 65, demographic = c())
age_standardised_rates_by_age <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age", age_cutoff = 65, demographic = c("age_grp_2"))
age_standardised_rates_by_sex <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age", age_cutoff = 65, demographic = c("sex"))
age_standardised_rates_by_simd <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age", age_cutoff = 65, demographic = c("simd"))

# Age and sex standardised rates
age_sex_standardised_rates <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c())
age_sex_standardised_rates_by_age <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c("age_grp_2"))
age_sex_standardised_rates_by_sex <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c("sex"))
age_sex_standardised_rates_by_simd <- create_rates_table(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c("simd"))

# Join data frames
standardised_rates <- full_join(age_sex_standardised_rates, age_standardised_rates)
standardised_rates_by_age <- full_join(age_sex_standardised_rates_by_age, age_standardised_rates_by_age)
standardised_rates_by_sex <- full_join(age_sex_standardised_rates_by_sex, age_standardised_rates_by_sex)
standardised_rates_by_simd <- full_join(age_sex_standardised_rates_by_simd, age_standardised_rates_by_simd)

################################################################################.
### 4. Calculate Other Measures ----
################################################################################.

# Function to create a table showing LDP, waiting times, uptake decision, contact, and rate by expected diagnoses
create_measures_table <- function(pds, exp_df, demographic = c()){
  measures_df <- pds %>%
    { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
    { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
    { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
    # Group by health board/ijb/scotland (geog/name/board), year (fy) and demographics(age_grp_2/sex/simd)
    group_by(geog, fy, name, board, across(any_of(demographic))) %>%
    summarise(
      # Referrals
      referrals = n(),
      # LDP Standard
      met                                                               = sum(str_detect(ldp, "complete"), na.rm = TRUE),
      not_met                                                           = sum(str_detect(ldp, "fail"), na.rm = TRUE),
      ongoing                                                           = sum(str_detect(ldp, "ongoing"), na.rm = TRUE),
      exempt                                                            = sum(str_detect(ldp, "exempt"), na.rm = TRUE),
      perc_met                                                          = round((met + exempt)/(met + exempt + not_met) * 100, 1),
      perc_not_met                                                      = round(not_met/referrals * 100, 1),
      perc_ongoing                                                      = round(ongoing/referrals * 100, 1),
      perc_exempt                                                       = round(exempt/referrals * 100, 1),
      `PDS terminated less than 11 months after first contact`          = round(sum(ldp == "fail - PDS terminated less than 11 months after first contact", na.rm = TRUE)/not_met * 100, 1),
      `PDS started more than 12 months after diagnosis`                 = round(sum(ldp == "fail - PDS started more than 12 months after diagnosis", na.rm = TRUE)/not_met * 100, 1),
      `PDS terminated before first contact`                             = round(sum(ldp == "fail - PDS terminated before first contact", na.rm = TRUE)/not_met * 100, 1),
      `PDS not started and more than 12 months since diagnosis`         = round(sum(ldp == "fail - PDS not started and more than 12 months since diagnosis", na.rm = TRUE)/not_met * 100, 1),
      `Still receiving PDS and less than 12 months since first contact` = round(sum(ldp == "ongoing - Still receiving PDS and less than 12 months since first contact", na.rm = TRUE)/ongoing * 100, 1),
      `PDS not started and less than 12 months since diagnosis`         = round(sum(ldp == "ongoing - PDS not started and less than 12 months since diagnosis", na.rm = TRUE)/ongoing * 100, 1),
      `Service user no longer able to engage in PDS`                    = round(sum(ldp == "exempt - 06 Service user no longer able to engage in PDS", na.rm = TRUE)/exempt * 100, 1),
      `Service user has moved to a different Health Board area`         = round(sum(ldp == "exempt - 04 Service user has moved to a different Health Board area", na.rm = TRUE)/exempt * 100, 1),
      `Service user has terminated PDS early/refused`                   = round(sum(ldp == "exempt - 05 Service user has terminated PDS early/refused", na.rm = TRUE)/exempt * 100, 1),
      `Service user has died`                                           = round(sum(ldp == "exempt - 03 Service user has died", na.rm = TRUE)/exempt * 100, 1),
      # Waiting Times
      perc_allocated         = round(sum(!is.na(initial_pds_practitioner_allocation_date), na.rm = TRUE)/referrals * 100, 1),
      perc_contacted         = round(sum(!is.na(date_of_initial_first_contact), na.rm = TRUE)/referrals * 100, 1),
      diagnosis_to_referral  = median(date_pds_referral_received - dementia_diagnosis_confirmed_date, na.rm = TRUE),
      referral_to_allocation = median(initial_pds_practitioner_allocation_date - date_pds_referral_received, na.rm = TRUE),
      allocation_to_contact  = median(date_of_initial_first_contact - initial_pds_practitioner_allocation_date, na.rm = TRUE),
      diagnosis_to_contact   = median(date_of_initial_first_contact - dementia_diagnosis_confirmed_date, na.rm = TRUE),
      # Uptake Decision
      uptake_decision      = sum(!is.na(pds_uptake_decision), na.rm = TRUE),
      perc_uptake_decision = round(uptake_decision/referrals * 100, 1),
      perc_accepted        = round(sum(pds_uptake_decision %in% c("01 Accepted", "03 Accepted, but Initially Declined"), na.rm = TRUE)/uptake_decision * 100, 1),
      # No Contact
      no_contact_12                   = sum(ldp %in% c("fail - PDS started more than 12 months after diagnosis", "fail - PDS not started and more than 12 months since diagnosis"), na.rm = TRUE),
      perc_no_contact_12              = round(no_contact_12/referrals * 100, 1),
      termination_before_contact      = sum(ldp == "fail - PDS terminated before first contact", na.rm = TRUE),
      perc_termination_before_contact = round(termination_before_contact/referrals * 100, 1),
      .groups = "drop") %>%
    # Add expected diagnoses data
    left_join(exp_df, by = c("geog", "fy")) %>%
    mutate(exp_rate = round(referrals / diagnoses * 100, 1)) 
  
  return(measures_df)
}

# Measures
measures <- create_measures_table(pds, exp_df)
measures_by_age <- create_measures_table(pds, exp_df, "age_grp_2")
measures_by_sex <- create_measures_table(pds, exp_df, "sex")
measures_by_simd <- create_measures_table(pds, exp_df, "simd")

################################################################################.
### 5 - Save data ----
################################################################################.

if (exists("save_output") && isTRUE(save_output)){

  write_file(standardised_rates, path = get_mi_data_path("standardised_rates", ext = "rds", test_output = test_output))
  write_file(standardised_rates_by_age, path = get_mi_data_path("standardised_rates_by_age", ext = "rds", test_output = test_output))
  write_file(standardised_rates_by_sex, path = get_mi_data_path("standardised_rates_by_sex", ext = "rds", test_output = test_output))
  write_file(standardised_rates_by_simd, path = get_mi_data_path("standardised_rates_by_simd", ext = "rds", test_output = test_output))
  
  write_file(measures, path = get_mi_data_path("measures", ext = "rds", test_output = test_output))
  write_file(measures_by_age, path = get_mi_data_path("measures_by_age", ext = "rds", test_output = test_output))
  write_file(measures_by_sex, path = get_mi_data_path("measures_by_sex", ext = "rds", test_output = test_output))
  write_file(measures_by_simd, path = get_mi_data_path("measures_by_simd", ext = "rds", test_output = test_output))
  
}

################################ END OF SCRIPT #################################.