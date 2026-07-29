################################################################################.
# Name of file - 01a_data-prep_population-files.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Abram McCormick
# Original Date - December 2024
# Updated by - Lucy Binsted
# Date - February 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - Create population lookup files.
################################################################################.

################################################################################.
### 1 - Load environment file ----
################################################################################.

source(here::here("code", "00_setup-environment.R"))

# Helper function to get the geography year e.g. HSCP2019, HB2019
# Enables dynamic column selection
get_pop_est_year <- function(path) {
  stringr::str_match(
    fs::path_file(path),
    "^[^0-9]+([0-9]{4})_"
  )[, 2]
}

################################################################################.
### 2 - Create population file from mid-year estimates ----
################################################################################.

## Creating population for IJB ----

la_pop_path <- get_pop_path(type = "HSCP", selection_method = "modification_date")

la_pop <- read_rds(la_pop_path) %>% 
  filter(year >= 2016, age >= 18)

geog_col_ijb <- paste0("hscp", get_pop_est_year(la_pop_path), "name")

# Create a new column with age in 8 groups
la_pop %<>%
  mutate(age_grp = case_when(
    age %in% 18:59 ~ "59 and Under",
    age %in% 60:64 ~ "60 to 64",
    age %in% 65:69 ~ "65 to 69",
    age %in% 70:74 ~ "70 to 74",
    age %in% 75:79 ~ "75 to 79",
    age %in% 80:84 ~ "80 to 84",
    age %in% 85:89 ~ "85 to 89",
    age >= 90      ~ "90+"
  )) %>%
  
  # Create a new column with age in 3 groups
   mutate(age_grp_2 = case_when(
     age %in% 65:79 ~ "79 and Under", # This is used for calculating rates where denominator is only 65 and over population
     age %in% 80:84 ~ "80 to 84",
     age >= 85     ~ "85+"
   )) %>%
  
  # Convert sex column from integer to string
   mutate(sex = case_when(
     sex == 1 ~ "01 Male",
     sex == 2 ~ "02 Female"
   ))

# Aggregate by year, LA, age group and gender
la_pop_data <- bind_rows(
  
    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2, age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2, age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),

    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2, age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2, age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"), 

    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2 = "All", age_grp, sex) %>%
      summarise(pop_est = sum(pop), .groups ="drop"),
    
    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2 = "All", age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2 = "All", age_grp, sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2 = "All", age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2 = "All", age_grp, sex = "All") %>%
      summarise(pop_est = sum(pop), .groups ="drop"),
    
    la_pop %>%
      group_by(geog = !!sym(geog_col_ijb), year, age_grp_2 = "All", age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2 = "All", age_grp, sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog = "Scotland", year, age_grp_2 = "All", age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop")
  )

# Clean up geography for Edinburgh and Western Isles
la_pop_data %<>% 
  mutate (geog = case_when(
    str_detect(geog, "Edinburgh") ~ "Edinburgh City",
    str_detect(geog, "Na h-Eileanan Siar") ~ "Western Isles",
    TRUE ~ geog
))

## Creating population for HB ----

hb_pop_path <- get_pop_path(type = "HB", selection_method = "modification_date")

hb_pop <- read_rds(hb_pop_path) %>%
  filter(year >= 2016, age >= 18)

geog_col_hb <- paste0("hb", get_pop_est_year(hb_pop_path), "name")

# Create a new column with age in 8 groups
hb_pop %<>%
  mutate(age_grp = case_when(
    age %in% 18:59 ~ "59 and Under",
    age %in% 60:64 ~ "60 to 64",
    age %in% 65:69 ~ "65 to 69",
    age %in% 70:74 ~ "70 to 74",
    age %in% 75:79 ~ "75 to 79",
    age %in% 80:84 ~ "80 to 84",
    age %in% 85:89 ~ "85 to 89",
    age >= 90      ~ "90+"
  )) %>%
  
  # Create a new column with age in 3 groups
  mutate(age_grp_2 = case_when(
    age %in% 65:79 ~ "79 and Under", # This is used for calculating rates where denominator is only 65 and over population
    age %in% 80:84 ~ "80 to 84",
    age >= 85     ~ "85+"
  )) %>%
  
  # Convert sex column from integer to string
  mutate(sex = case_when(
    sex == 1 ~ "01 Male",
    sex == 2 ~ "02 Female"
  ))

# Aggregate by year, hb, age group and gender
hb_pop_data <- bind_rows(
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2, age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2 = "All", age_grp, sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2 = "All", age_grp = "All", sex) %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2, age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2 = "All", age_grp, sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog = !!sym(geog_col_hb), year, age_grp_2 = "All", age_grp = "All", sex = "All") %>%
      summarise(pop_est = sum(pop), .groups = "drop") 
  )

# Clean up geography for Ayrshire, D&G and Glasgow
hb_pop_data %<>% 
  mutate (geog = case_when(
    str_detect(geog, "NHS Ayrshire and Arran") ~ "NHS Ayrshire & Arran",
    str_detect(geog, "NHS Dumfries and Galloway") ~ "NHS Dumfries & Galloway",
    str_detect(geog, "NHS Greater Glasgow and Clyde") ~ "NHS Greater Glasgow & Clyde",
    TRUE ~ geog
  ))

## Merge and save population for IAA and population for HB ----

# Merge LA and HB pops
pop_data <-bind_rows(la_pop_data, hb_pop_data)

# Add missing years until the current year by duplicating the latest year in pop_data
while(max(pop_data$year) < fy){
  pop_data %<>% rbind((pop_data %>% filter(year == max(year)) %>% mutate(year = max(year) + 1)))
}

# Check
tabyl(pop_data$geog)

# Save file
pop_data %>% 
  write_file(
    path = get_pop_lookup_path(
      check_mode = "write",
      create_dir = TRUE))
#0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

################################################################################.
### 3 - Read in SIMD population file ---- 
################################################################################.

simd_pop_path <- get_pop_path(type = "DataZone", selection_method = "modification_date")

simd_pop <- read_rds(simd_pop_path) %>% 
  filter(year >= 2016)
  
simd_pop_la <- simd_pop %>%
  select(geog = !!sym(geog_col_ijb), year, simd = simd2020v2_sc_quintile, sex, 5:95) 

simd_pop_hb <- simd_pop %>%
  select(geog = !!sym(geog_col_hb), year, simd = simd2020v2_sc_quintile, sex, 5:95)

simd_pop_16_22 <- bind_rows(simd_pop_la,simd_pop_hb)

simd_pop_16_22 %<>% pivot_longer(
  cols = 5:95,
  names_to = "age",
  values_to = "pop"
) %>% 
  mutate(age = as.numeric(substring(age,4,5))) %>% 
  filter(age >= 18)

simd_pop_16_22 %<>% mutate(simd = case_when(
  simd == 1 ~ "1 - Most Deprived",
  simd == 2 ~ "2",
  simd == 3 ~ "3",
  simd == 4 ~ "4",
  simd == 5 ~ "5 - Least Deprived"
))

simd_pop_data <- bind_rows(
  
  simd_pop_16_22 %>% 
    group_by(year, geog, simd, sex, age) %>% 
    summarise(pop = sum(pop), .groups = "drop"),
  
  simd_pop_16_22 %>% 
    group_by(year, geog, simd, sex = "All", age) %>% 
    summarise(pop = sum(pop), .groups = "drop"),
  
  simd_pop_16_22 %>% 
    filter(grepl("NHS", geog)) %>% 
    group_by(year, geog = "Scotland", simd, sex, age) %>% 
    summarise(pop = sum(pop), .groups = "drop"),
  
  simd_pop_16_22 %>% 
    filter(grepl("NHS", geog)) %>% 
    group_by(year, geog = "Scotland", simd, sex = "All", age) %>% 
    summarise(pop = sum(pop), .groups = "drop"))

# Create a new column with age in 8 groups
simd_pop_data %<>% 
  mutate(age_grp = case_when(
    age %in% 18:59 ~ "59 and Under",
    age %in% 60:64 ~ "60 to 64",
    age %in% 65:69 ~ "65 to 69",
    age %in% 70:74 ~ "70 to 74",
    age %in% 75:79 ~ "75 to 79",
    age %in% 80:84 ~ "80 to 84",
    age %in% 85:89 ~ "85 to 89",
    age >= 90      ~ "90+"
  )) %>% 
  
  # Create a new column with age in 3 groups
  mutate(age_grp_2 = case_when(
    age %in% 65:79 ~ "79 and Under", # This is used for calculating rates where denominator is only 65 and over population
    age %in% 80:84 ~ "80 to 84",
    age >= 85     ~ "85+"
  )) %>%

  # Convert sex column from integer to string
  mutate(sex = case_when(
    sex == "M" ~ "01 Male",
    sex == "F" ~ "02 Female",
    sex == "All" ~ "All"
  ))

# Aggregate by year, HB/IAA, SIMD, age and gender
simd_pop_data %<>% 
  group_by(geog, year, age_grp_2, age_grp, sex, simd) %>% 
  summarise(pop = sum(pop), .groups = "drop")

# Aggregate by year, HB/IAA, SIMD, age and gender
simd_pop_summary <- bind_rows(
  
  simd_pop_data,
  
  simd_pop_data %>% 
    group_by(geog, year, age_grp_2, age_grp = "All", sex, simd) %>% 
    summarise(pop = sum(pop), .groups = "drop"),
  
  simd_pop_data %>% 
    group_by(geog, year, age_grp_2 = "All", age_grp, sex, simd) %>% 
    summarise(pop = sum(pop), .groups = "drop"),
  
  simd_pop_data %>% 
    group_by(geog, year, age_grp_2 = "All", age_grp = "All", sex, simd) %>% 
    summarise(pop = sum(pop), .groups = "drop")
  )

# Clean up geography for Ayrshire, D&G, Glasgow, Edinburgh and Western Isles
simd_pop_summary %<>% 
  mutate (geog = case_when(
    str_detect(geog, "NHS Ayrshire and Arran") ~ "NHS Ayrshire & Arran",
    str_detect(geog, "NHS Dumfries and Galloway") ~ "NHS Dumfries & Galloway",
    str_detect(geog, "NHS Greater Glasgow and Clyde") ~ "NHS Greater Glasgow & Clyde",
    str_detect(geog, "Edinburgh") ~ "Edinburgh City",
    str_detect(geog, "Na h-Eileanan Siar") ~ "Western Isles",
    TRUE ~ geog
  ))

# Add missing years until the current year by duplicating the latest year in simd_pop_summary
while(max(simd_pop_summary$year) < fy){
  simd_pop_summary %<>% rbind((simd_pop_summary %>% filter(year == max(year)) %>% mutate(year = max(year) + 1)))
}

simd_pop_data_final <- simd_pop_summary %>%
  complete(nesting(year, geog, age_grp, age_grp_2, sex), simd, fill = list(pop = 0)) 

simd_pop_data_final %>% 
  write_file(
    path = get_pop_lookup_path(
      simd = TRUE,
      check_mode = "write",
      create_dir = TRUE))
#0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

################################ END OF SCRIPT #################################.