################################################################################.
# Name of file - 01_update-lookups.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Abram McCormick
# Original Date - December 2024
# Updated by Lucy Binsted - June 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - 
#   Detect and read in the most up-to-date SIMD and population files
#   Get the European Standard Population Open Data
#   Read the expected diagnoses file
#   Process lookups
################################################################################.

################################################################################.
### 1. Setup ----
################################################################################.

# Source the setup script
source(here::here("code", "00_setup-environment.R"))

################################################################################.
### 2. Get up-to-date paths ----
################################################################################.

# SIMD Lookup Path
simd_path <- get_simd_path(
    selection_method = "modification_date")

# Population Lookup Path (Health Boards)
pop_path_hb <- get_pop_path(
  type = "HB", 
  selection_method = "modification_date")

# Population Lookup Path (HSCPs)
pop_path_hscp <- get_pop_path(
  type = "HSCP", 
  selection_method = "modification_date")

# Population Lookup Path (SIMD)
pop_path_simd <- get_pop_path(
  type = "DataZone", 
  selection_method = "modification_date",
  choose = TRUE)
4
################################################################################.
### 3. Read data ----
################################################################################.

# SIMD Lookup
simd_lookup <- read_rds(simd_path)

# Population Lookup (Health Boards)
pop_lookup_hb <- read_rds(pop_path_hb)

# Population Lookup (HSCPs)
pop_lookup_hscp <- read_rds(pop_path_hscp)

# Population Lookup (SIMD)
pop_lookup_simd <- read_rds(pop_path_simd)

# Expected Diagnoses
exp_df <- read_csv(get_exp_diagnoses_path())

# European Standard Population
ESP13_df <- phsopendata::get_resource(res_id = "29ce4cda-a831-40f4-af24-636196e05c1a")

################################################################################.
### 4. Process SIMD Lookup ----
################################################################################.

simd_lookup <- simd_lookup %>% 
  clean_names() %>%
  select(pc7, simd = simd2020v2_sc_quintile) %>%
  mutate(
    simd = case_when(
      simd == 1 ~ "1 - Most Deprived",
      simd == 5 ~ "5 - Least Deprived",
      TRUE ~ as.character(simd)
    )
  )

################################################################################.
### 5. Process Population Lookups ----
################################################################################.

# Helper function to get the name of the latest geography/SIMD column
get_geog_col <- function(data, type){
  pattern <- dplyr::recode_values(
    type,
    "hb"   ~ "^hb\\d{4}name$",
    "hscp" ~ "^hscp\\d{4}name$",
    "simd" ~ "^simd\\d{4}v\\d+_sc_quintile$")
  matches <- str_subset(names(data), pattern)
  nums <- str_extract_all(matches, "\\d+")
  num_mat <- do.call(rbind, lapply(nums, as.numeric))
  latest <- do.call(order, c(as.data.frame(num_mat), decreasing = TRUE))[1]
  matches[latest]
}

# Helper function to clean the population lookups
clean_pop_lookup <- function(pop_lookup){
  pop_lookup <- pop_lookup %>%
    # Remove years before 2016
    filter(fy >= 2016) %>%
    mutate(
      # Create a new column called scot (used for calculating scotland totals)
      scot = "Scotland", 
      # Replace 'and' with ampersand in health boards
      across(any_of("health_board"), ~ str_replace(
        .x, " and ", " & "
      )),
      # Clean up Edinburgh and Western Isles IJB
      across(any_of("ijb"), ~ case_when(
        str_detect(.x, "Edinburgh") ~ "Edinburgh City",
        str_detect(.x, "Na h-Eileanan Siar") ~ "Western Isles",
        TRUE ~ .x
      )),
      # Convert simd column from integer to string
      across(any_of("simd"), ~ case_when(
        .x == 1 ~ "1 - Most Deprived",
        .x == 5 ~ "5 - Least Deprived",
        TRUE ~ as.character(.x)
      )),
      # Create 5-year age groups
      age_grp = case_when(
        age < 90 ~ paste0((age %/% 5) * 5, "-", (age %/% 5) * 5 + 4, " years"),
        age >= 90 ~ "90plus years"
      ),
      # Create broad age groups
      age_grp_2 = case_when(
        age <= 0 | is.na(age) ~ "Unknown",
        age %in% 1:79 ~ "79 and Under",
        age %in% 80:84 ~ "80 to 84",
        age >= 85     ~ "85+"
      ),
      # Convert sex column from integer to string
      sex = case_when(
        sex == "M" ~ "01 Male",
        sex == "F" ~ "02 Female",
        TRUE ~ sex
      )) %>%
    # Put health board and ijb in the same column named geog
    pivot_longer(
      cols = all_of(intersect(c("scot", "ijb", "health_board"), names(.))),
      values_to = "geog"
    ) %>%
    group_by(across(all_of(intersect(c("geog", "name", "fy", "age", "age_grp", "age_grp_2", "sex", "simd"), names(.))))) %>%
    summarise(
      population_estimate = sum(population_estimate),
      .groups = "drop"
    )
  
  # Add missing years until the current year by duplicating the latest year in pop_lookup_simd
  pop_lookup$duplicated_year <- FALSE
  start_fy <- max(pop_lookup$fy)
  
  while(max(pop_lookup$fy) < as.numeric(fy)){
    pop_lookup %<>% rbind((
      pop_lookup %>% 
        filter(fy == max(fy)) %>% 
        mutate(fy = max(fy) + 1,
               duplicated_year = TRUE)))
  }
  
  end_fy <- max(pop_lookup$fy)
  if (end_fy > start_fy) {
    message(
      "Population data from ", start_fy,
      " carried for ",
      paste((start_fy + 1):end_fy, collapse = ", "))
  }
 
  # Convert fy column from YYYY to "YYYY/YY"
  pop_lookup <- pop_lookup %>%
    mutate(fy = paste0(fy, "/", sprintf("%02d", (fy + 1) %% 100)))
  
  return(pop_lookup)
}

# Process latest file corresponding to HB estimates
pop_lookup_hb <- pop_lookup_hb %>%
  select(health_board = !!sym(get_geog_col(pop_lookup_hb, "hb")), 
         fy = year, 
         age = age,
         sex = sex_name,
         population_estimate = pop) %>%
  clean_pop_lookup()

# Process latest file corresponding to HSCP estimates
pop_lookup_hscp <- pop_lookup_hscp %>% 
  select(ijb = !!sym(get_geog_col(pop_lookup_hscp, "hscp")), 
         fy = year, 
         age = age,
         sex = sex_name,
         population_estimate = pop) %>%
  clean_pop_lookup()

# Process latest file corresponding to SIMD estimates
pop_lookup_simd <- pop_lookup_simd %>% 
  # Pivot longer so age is in a single column
  pivot_longer(cols = matches("^age[0-9]+"), names_to = "age", values_to = "population_estimate") %>% 
  # Group by health board, ijb, fy, simd, sex and age, calculate total population and add missing combinations
  group_by(health_board = !!sym(get_geog_col(pop_lookup_simd, "hb")), 
           ijb = !!sym(get_geog_col(pop_lookup_simd, "hscp")), 
           simd = !!sym(get_geog_col(pop_lookup_simd, "simd")), 
           fy = year, 
           age =  as.numeric(gsub("\\D", "", age)),
           sex = sex) %>% 
  summarise(population_estimate = sum(population_estimate), .groups = "drop") %>%
  complete(nesting(ijb, health_board), fy, age, sex, simd, fill = list(population_estimate = 0)) %>%
  clean_pop_lookup()

################################################################################.
### 6. Process Expected Diagnoses ----
################################################################################.

# Expected Diagnoses data
exp_df <- exp_df %>%
  # Rename health board column to geog to match PDS and population data
  rename(geog = health_board_label) %>% 
  # Remove health board code column
  select(-health_board) 

################################################################################.
### 7. Process ESP ----
################################################################################.

# European Standard Population
ESP13_df <- ESP13_df %>%
  # Convert column names to snake case
  clean_names() %>%
  # Remove code
  mutate(sex = case_when(
    sex == "Male" ~ "01 Male",
    sex == "Female" ~ "02 Female")) %>%
  rename(age_grp = age_group)

################################################################################.
### 8. Lookup Information (SIMD and populations) ----
################################################################################.

# # Helper function to get lookup information
# create_info_row <- function(path, lookup, type, fy){
#   if("actual_year" %in% names(lookup)) {
#     data_from <- max(lookup$actual_year, na.rm = TRUE)
#     carried_to <- paste((data_from + 1):as.numeric(fy), collapse = ", ")
#   } else {
#     data_from <- NA
#     carried_to <- NA
#   }
#   info <- fs::file_info(path) %>%
#     select(path, size, modification_time) %>%
#     mutate(
#       lookup = type,
#       data_from = data_from,
#       carried_to = carried_to
#     )
#   info
# }
# 
# # Lookup information
# lookup_info <- bind_rows(
#   create_info_row(simd_path, simd_lookup, "simd_geography", fy),
#   create_info_row(pop_path_hb, pop_lookup_hb, "hb_population", fy),
#   create_info_row(pop_path_hscp, pop_lookup_hscp, "hscp_population", fy),
#   create_info_row(pop_path_simd, pop_lookup_simd, "simd_population", fy)
# )

# Helper function to get lookup information
create_info_row <- function(path, lookup, type){
  if(grepl("population", type)) {
    actual_year_list <- as.integer(sub("/.*", "", unique((lookup %>% filter(duplicated_year == FALSE))$fy)))
    data_from <- max(actual_year_list)
    duplicated_year_list <- unique((lookup %>% filter(duplicated_year == TRUE))$fy)
    carried_to <- glue::glue_collapse(duplicated_year_list, sep = ", ", last = " and ")
  } else {
    data_from <- NA
    carried_to <- NA
  }
  info <- fs::file_info(path) %>%
    select(path, size, modification_time) %>%
    mutate(
      lookup = type,
      data_from = data_from,
      carried_to = carried_to
    )
  info
}

# Lookup information
lookup_info <- bind_rows(
  create_info_row(simd_path, simd_lookup, "simd_geography"),
  create_info_row(pop_path_hb, pop_lookup_hb, "hb_population"),
  create_info_row(pop_path_hscp, pop_lookup_hscp, "hscp_population"),
  create_info_row(pop_path_simd, pop_lookup_simd, "simd_population")
)

################################################################################.
### 9. Save data ----
################################################################################.

# Save lookup information to MI data path
lookup_info %>% 
  write_file(path = get_mi_data_path(
    type = "lookup_info", 
    ext = "csv", 
    fy = fy,
    qt = qt,
    test_output = test_output,
    check_mode = "write",
    create_dir = TRUE))

# Save lookup information to lookup path
lookup_info %>% 
  write_file(path = get_lookup_path(
    type = "lookup_info", 
    check_mode = "write",
    create_dir = TRUE))

# Save SIMD lookup
simd_lookup %>% 
  write_file(path = get_lookup_path(
    type = "simd",
    check_mode = "write",
    create_dir = TRUE))

# Save HB/HSCP population lookup
pop_lookup <- bind_rows(pop_lookup_hb, pop_lookup_hscp) %>% 
  distinct() 

pop_lookup %>%
  write_file(path = get_lookup_path(
    type = "pop", 
    check_mode = "write",
    create_dir = TRUE))

# Save SIMD population lookup
pop_lookup_simd %>% 
  write_file(path = get_lookup_path(
    type = "pop_simd", 
    check_mode = "write",
    create_dir = TRUE))

# Save expected diagnoses
exp_df %>% 
  write_file(path = get_lookup_path(
    type = "exp",
    check_mode = "write",
    create_dir = TRUE))

# Save ESP
ESP13_df %>% 
  write_file(path = get_lookup_path(
    type = "esp",
    check_mode = "write",
    create_dir = TRUE))

################################ END OF SCRIPT #################################.