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
# Description - Create population lookup files. Only needs to be run when updates are available.
################################################################################.

### 0 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))

### 1 - Read latest files ----

# List all files in the folder
pop_filepath <- glue("{cl_out}/lookups/Unicode/Populations/Estimates/")
pop_files <- list.files(pop_filepath)

# Get files corresponding to IAA, Health Board and SIMD estimates
ijb_files <- pop_files[grepl("^HSCP\\d+_pop_est_\\d+_\\d+\\.rds$", pop_files)]
hb_files <- pop_files[grepl("^HB\\d+_pop_est_\\d+_\\d+\\.rds$", pop_files)]
simd_files <- pop_files[grepl("^DataZone\\d+_pop_est_\\d+_\\d+\\.rds$", pop_files)]

# Function which takes a list of strings containing dates and returns the most recent
get_latest <- function(list, name, type){
  latest <- as_tibble(do.call(rbind, str_extract_all(list, "\\d+"))) %>% # Extract numbers from each string as separate columns of a tibble
    mutate(across(everything(), as.numeric), string = list) %>% # Convert to numeric and add new column containing original string in 
    arrange(across(everything(), desc)) %>% # Sort by numbers, highest at the top (first number is most important, then second etc.)
    slice(1) %>% # Get the first row
    pull(string) # Get tje original string
  message(paste0("The following ", name, " population ", type, "s are available:\n", paste(list, collapse = "\n"), 
                 "\n\nSelected ", type, ":\n", latest)) # Print a message showing the choices available and the selection that has been made so the analyst can check
  return (latest) # Return latest
}

ijb_file <- get_latest(ijb_files, "IAA", "file")
hb_file <- get_latest(hb_files, "Health Board", "file")
simd_file <- get_latest(simd_files, "SIMD", "file")

# Read IAA file and remove rows before 2016 or younger than 18
la_pop <- read_rds(glue(pop_filepath, ijb_file))%>% 
  filter(year >= 2016, age >= 18)

# Read IAA file and remove rows before 2016 or younger than 18
hb_pop <- read_rds(glue(pop_filepath, hb_file)) %>%
  filter(year >= 2016, age >= 18)

# Read SIMD file, remove rows before 2016
simd_pop <- read_rds(glue(pop_filepath, simd_file)) %>% 
  filter(year >= 2016) 

# Define columns which will be renamed to `geog` or `simd`
geog_cols_ijb <- grep("hscp[0-9].*name", colnames(la_pop), value = TRUE)
geog_cols_hb <- grep("hb[0-9].*name", colnames(hb_pop), value = TRUE)
simd_cols <- sort(grep("simd[0-9].*_sc_quintile", colnames(simd_pop), value = TRUE))

# Select files
geog_col_ijb <- get_latest(geog_cols_ijb, "IAA", "column")
geog_col_hb <- get_latest(geog_cols_hb, "Health Board", "column")
simd_col <- get_latest(simd_cols, "SIMD", "column")

### 2 - Create population file from mid-year estimates ----

# Creating population for IAA ----

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

# Creating population for HB ----

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

# Merge and save population for IAA and population for HB ----

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
  write_file(path = "//conf/dementia/A&I/Outputs/management-report/lookups/pop_data.rds")
0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

### 3 - Create SIMD population file ---- 

# Create IAA SIMD populations
simd_pop_la <- simd_pop %>% 
  select(geog = !!sym(geog_col_ijb), year, simd = !!sym(simd_col), sex, matches("^age[0-9]+"))

# Create HB SIMD populations
simd_pop_hb <- simd_pop %>% 
  select(geog = !!sym(geog_col_hb), year, simd = !!sym(simd_col), sex, matches("^age[0-9]+"))

# Merge IAA and HB SIMD populations
simd_pop_16_22 <- bind_rows(simd_pop_la, simd_pop_hb)

# Pivot to long, convert age to numeric and remove rows younger than 18
simd_pop_16_22 %<>% 
  pivot_longer(cols = matches("^age[0-9]+"), names_to = "age", values_to = "pop") %>% 
  mutate(age = as.numeric(gsub("\\D", "", age))) %>% 
  filter(age >= 18)

# Convert simd column from integer to string
simd_pop_16_22 %<>% 
  mutate(simd = case_when(
    simd == 1 ~ "1 - Most Deprived",
    simd == 2 ~ "2",
    simd == 3 ~ "3",
    simd == 4 ~ "4",
    simd == 5 ~ "5 - Least Deprived"
    ))

# Aggregate by year, HB/IAA, SIMD, age and gender
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

# Fill missing SIMD with population 0
simd_pop_data_final <- simd_pop_summary %>% 
  complete(nesting(year, geog, age_grp, age_grp_2, sex), simd, fill = list(pop = 0)) 
 
# Save file
simd_pop_data_final %>% 
  write_file(path = "//conf/dementia/A&I/Outputs/management-report/lookups/simd_pop_data.rds")
0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

### END OF SCRIPT