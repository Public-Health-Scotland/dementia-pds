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
    group_by(fy, geog, name, board, age_grp, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total referrals
    summarise(
      # Number of referrals
      all_referrals = n(), 
      over_65_referrals = sum(age >= 65, na.rm = T),
      # Number of referrals above the age cutoff (age standardised) or above the age cutoff with known sex (age-sex standardised)
      filtered_referrals = sum(age >= age_cutoff & !(sex_dummy %in% c("98 Not Specified", "99 Not Known")), na.rm = TRUE),
      .groups = "drop")
  
  # 2. Prepare population data
  population <- population_df %>% 
    # Add health board colunm
    left_join(pds %>% distinct(geog, board)) %>% 
    # Create a dummy sex column to allow switching between age and age-sex standardisation
    { if (standardisation == "age-sex") mutate(., sex_dummy = sex) else mutate(., sex_dummy = 1) } %>%
    # Remove unknown age/sex/simd if they are the demographic
    { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
    { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
    { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name), 5-year age group (age_grp), sex if age-sex standardisation (sex_dummy) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, board, age_grp, sex_dummy, across(any_of(demographic))) %>%
    # Calculate total population
    summarise(
      total_population = sum(population_estimate, na.rm = TRUE),
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
    mutate(
      age_start = as.numeric(str_extract(age_grp, "^\\d+")),
      age_end = as.numeric(str_extract(age_grp, "(?<=-)\\d+")),
      proportion = case_when(
        age_start >= age_cutoff ~ 1,
        age_end < age_cutoff ~ 0,
        TRUE ~ (age_end - age_cutoff + 1) / (age_end - age_start + 1)
      ),
      filtered_esp = european_standard_population * proportion
    ) %>%
    select(-age_start, -age_end, -proportion)
  
  # 4. Join data and calculate rates
  rates_df <- full_join(pds, population, by = c("fy", "geog", "name", "board", "age_grp", "sex_dummy", demographic)) %>%
    left_join(ESP13, by = c("age_grp", "sex_dummy")) %>%
    # Population is complete, pds is not. This converts NA referrals to 0 for calculations
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    # Group by year (fy), ijb/health_board/scotland (geog/name) and demographics (age_grp_2/sex/simd)
    group_by(fy, geog, name, board, across(any_of(demographic))) %>%
    summarise(
      
      # Total referrals (uncomment to check calculations)
      # filtered_referrals = sum(filtered_referrals),
      # all_referrals = sum(all_referrals),
      # total_population = sum(total_population),
      # filtered_population = sum(filtered_population),
      # over_65_population = sum(over_65_population),
      
      # Age-sex standardised rate using referrals and population estimates over 18 (per 100,000)
      standardised_rate = ifelse(
        sum(filtered_population) == 0,
        NA,
        sum(filtered_referrals / filtered_population * filtered_esp, na.rm = TRUE) * 100000 / sum(filtered_esp)),
      # Crude rate using all referrals and population estimates over 65 (per 10,000)
      rate = (sum(all_referrals, na.rm = TRUE) / sum(over_65_population)) * 10000,
      .groups = "drop") %>%
    # Convert NaN and Inf to NA
    mutate(across(everything(), ~ replace(.x, is.nan(.x)|is.infinite(.x), NA)))
  
  return(rates_df)
}
