create_rates_table <- function(pds, population_df, ESP13_df, demographic = c(), standardisation = "age-sex", age_cutoff = 18){
  
  # 1. Prepare LDP data
  pds <- pds %>% 
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Remove unknown age/sex/simd if they are the demographic
    { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
    { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
    { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_grp), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, age_grp, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total referrals
    summarise(
      # Number of referrals
      all_referrals = n(), 
      # Number of referrals above the age cutoff (age standardised) or above the age cutoff with known sex (age-sex standardised)
      filtered_referrals = sum(age >= age_cutoff & !(sex_dummy %in% c("98 Not Specified", "99 Not Known")), na.rm = TRUE),
      .groups = "drop")
  
  # 2. Prepare population data
  population <- population_df %>% 
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_grp), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, age_grp, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total population
    summarise(
      # Over 65 population
      over_65_population = sum(population_estimate[age >= 65], na.rm = TRUE),
      # Population above the age cutoff
      filtered_population = sum(population_estimate[age >= age_cutoff], na.rm = TRUE),
      .groups = "drop")
  
  # 3. Prepare ESP13 data
  ESP13 <- ESP13_df %>%
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Remove original sex column and duplicate rows (necessary for age standardisation)
    select(-sex) %>% distinct() %>%
    # Create a column that only contains ESP for age groups above the age cutoff
    mutate(filtered_esp = ifelse(as.numeric(str_extract(age_grp, "^\\d+")) >= age_cutoff, european_standard_population, 0))
  
  # 4. Join data and calculate rates
  rates_df <- left_join(pds, population, by = c("fy", "geog", "name", "age_grp", "sex_dummy", demographic)) %>%
    left_join(ESP13, by = c("age_grp", "sex_dummy")) %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, across(any_of(demographic))) %>%
    summarise(
      # Age-sex standardised rate using referrals and population estimates over 18 (per 100,000)
      standardised_rate = sum(filtered_referrals / filtered_population * filtered_esp, na.rm = TRUE) * 100000 / sum(filtered_esp, na.rm = TRUE),
      # Crude rate using all referrals and population estimates over 65 (per 10,000)
      old_rate = sum(all_referrals, na.rm = TRUE) / sum(over_65_population, na.rm = TRUE) * 10000,
      .groups = "drop") %>%
    
    return(rates_df)
}

# # Function to create a table showing referral rates per 10000 population
# create_rates_table <- function(pds, population_df, ESP13_df, standardisation = "age-sex", age_cutoff = 65, demographic = c()){
#   
#   # 1. Prepare LDP data
#   pds <- pds %>% 
#     # Create a dummy sex column to allow switching between age and age-sex standardisation
#     { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
#     # Remove unknown age/sex/simd if they are the demographic
#     { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
#     { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
#     { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
#     # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_grp), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
#     group_by(fy, geog, name, age_grp, sex_dummy, across(any_of(demographic))) %>%
#     # Calculate total referrals
#     summarise(
#       # Number of referrals
#       all_referrals = n(), 
#       # Number of referrals above the age cutoff
#       age_cutoff_referrals = sum(age >= age_cutoff, na.rm = TRUE),
#       # Number of referrals with known age (age standardised) or with known age and sex (age-sex standardised)
#       filtered_referrals = sum(age_grp != "Unknown" & !sex_dummy %in% c("98 Not Specified", "99 Not Known")),
#       # Number of referrals above the age cutoff (age standardised) or above the age cutoff with known sex (age-sex standardised)
#       age_cutoff_filtered_referrals = sum(age >= age_cutoff & !sex_dummy %in% c("98 Not Specified", "99 Not Known"), na.rm = TRUE),
#       .groups = "drop")
#   
#   # 2. Prepare population data
#   population <- population_df %>% 
#     # Create a dummy sex column to allow switching between age and age-sex standardisation
#     { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
#     # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_grp), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
#     group_by(fy, geog, name, age_grp, sex_dummy, across(any_of(demographic))) %>%
#     # Calculate total population
#     summarise(
#       # Population
#       all_population = sum(population_estimate, na.rm = TRUE),
#       # Population above the age cutoff
#       age_cutoff_population = sum(population_estimate[age >= age_cutoff], na.rm = TRUE),
#       .groups = "drop")
#   
#   # 3. Prepare ESP13 data
#   ESP13 <- ESP13_df %>%
#     # Create a dummy sex column to allow switching between age and age-sex standardisation
#     { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
#     # Remove original sex column and duplicate rows (necessary for age standardisation)
#     select(-sex) %>% distinct() %>%
#     # Create a column that only contains ESP for age groups above the age cutoff
#     mutate(age_cutoff_esp = ifelse(as.numeric(str_extract(age_grp, "^\\d+")) >= age_cutoff, european_standard_population, 0))
#   
#   # 4. Join data and calculate rates
#   rates_df <- full_join(pds, population, by = c("fy", "geog", "name", "age_grp", "sex_dummy", demographic)) %>%
#     left_join(ESP13, by = c("age_grp", "sex_dummy")) %>%
#     # Group by year (fy), ijb/health_board/scotland (geog/name) and demographics (age_grp_2/sex/simd)
#     group_by(fy, geog, name, across(any_of(demographic))) %>%
#     summarise(
#       
#       total_referrals                     = sum(all_referrals, na.rm = TRUE),
#       total_filtered_referrals            = sum(filtered_referrals, na.rm = TRUE),
#       total_age_cutoff_referrals          = sum(age_cutoff_referrals, na.rm = TRUE),
#       total_age_cutoff_filtered_referrals = sum(age_cutoff_filtered_referrals, na.rm = TRUE),
#       
#       total_population                    = sum(all_population, na.rm = TRUE),
#       total_age_cutoff_population         = sum(age_cutoff_population, na.rm = TRUE),
#       
#       crude_rate                          = total_referrals / total_population * 10000,
#       crude_rate_filtered                 = total_filtered_referrals / total_population * 10000,
#       crude_rate_age_cutoff               = total_age_cutoff_referrals / total_age_cutoff_population * 10000,
#       crude_rate_age_cutoff_filtered      = total_age_cutoff_filtered_referrals / total_age_cutoff_population * 10000,
#       
#       standardised_rate                   = sum(filtered_referrals / all_population * european_standard_population, na.rm = TRUE) * 10000 / sum(european_standard_population, na.rm = TRUE),
#       standardised_rate_age_cutoff        = sum(age_cutoff_filtered_referrals / age_cutoff_population * age_cutoff_esp, na.rm = TRUE) * 10000 / sum(age_cutoff_esp, na.rm = TRUE),
#       
#       old_rate                            = total_referrals / total_age_cutoff_population * 10000,
#       .groups = "drop") %>%
#     
#     return(rates_df)
# }
