################################################################################.
# Name of file - 02_create-population-lookup.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Abram McCormick
# Original Date - December 2024
# Updated by - Lucy Binsted
# Date - June 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - 
#   Detect and read in the most up-to-date SIMD population file
#   Process and save population data
################################################################################.

################################################################################.
### 1. Setup ----
################################################################################.

# Source the setup script
source(here::here("code", "00_setup-environment.R"))

################################################################################.
### 2. Read latest SIMD population file ----
################################################################################.

# Function which takes a list of strings containing dates and returns the most recent
get_latest <- function(file_list, name, type){
  latest <- as_tibble(do.call(rbind, str_extract_all(file_list, "\\d+")), .name_repair = "unique") %>% # Extract numbers from each string as separate columns of a tibble
    mutate(across(everything(), as.numeric), string = file_list) %>% # Convert to numeric and add new column containing original string in 
    arrange(across(everything(), desc)) %>% # Sort by numbers, highest at the top (first number is most important, then second etc.)
    slice(1) %>% # Get the first row
    pull(string) # Get tje original string
  message(paste0("The following ", name, " population ", type, "s are available:\n", paste(file_list, collapse = "\n"), 
                 "\n\nSelected ", type, ":\n", latest)) # Print a message showing the choices available and the selection that has been made so the analyst can check
  return (latest) # Return latest
}

# List all files in the population lookup folder
pop_filepath <- glue("{cl_out}/lookups/Unicode/Populations/Estimates/")
pop_files <- list.files(pop_filepath)

# List files corresponding to SIMD estimates
simd_files <- pop_files[grepl("^DataZone\\d+_pop_est_\\d+_\\d+\\.rds$", pop_files)]

# Select latest file corresponding to SIMD estimates
simd_file <- get_latest(simd_files, "SIMD", "file")

# Read latest file corresponding to SIMD estimates
simd_pop <- read_rds(glue(pop_filepath, simd_file))

# List all ijb, health_board and simd columns
geog_cols_ijb <- grep("hscp[0-9].*name", colnames(simd_pop), value = TRUE)
geog_cols_hb <- grep("hb[0-9].*name", colnames(simd_pop), value = TRUE)
simd_cols <- sort(grep("simd[0-9].*_sc_quintile", colnames(simd_pop), value = TRUE))

# Select latest ijb, health_board and simd columns
geog_col_ijb <- get_latest(geog_cols_ijb, "IAA", "column")
geog_col_hb <- get_latest(geog_cols_hb, "Health Board", "column")
simd_col <- get_latest(simd_cols, "SIMD", "column")

################################################################################.
### 3. Process SIMD population file ----
################################################################################.

# Process latest file corresponding to SIMD estimates
population_df <- simd_pop %>% 
  # Remove years before 2016
  filter(year >= 2016) %>%
  # Pivot longer so age is in a single column
  pivot_longer(cols = matches("^age[0-9]+"), names_to = "age", values_to = "population_estimate") %>% 
  # Group by health board, ijb, fy, simd, sex and age and calculate total population
  group_by(health_board = !!sym(geog_col_hb), 
           ijb = !!sym(geog_col_ijb), 
           fy = year, 
           simd = !!sym(simd_col), 
           sex, 
           age) %>% 
  summarise(population_estimate = sum(population_estimate), .groups = "drop") %>%
  # Add missing combinations
  complete(nesting(ijb, health_board), fy, age, sex, simd,
           fill = list(population_estimate = 0)) %>%
  mutate(
    # Create a new column called scot (used for calculating scotland totals)
    scot = "Scotland",
    # Replace 'and' with ampersand in health boards
    health_board = str_replace(health_board, " and ", " & "),
    # Clean up Edinburgh and Western Isles
    ijb = case_when(
      str_detect(ijb, "Edinburgh") ~ "Edinburgh City",
      str_detect(ijb, "Na h-Eileanan Siar") ~ "Western Isles",
      TRUE ~ ijb),
    # Convert simd column from integer to string
    simd = case_when(
      simd == 1 ~ "1 - Most Deprived",
      simd == 2 ~ "2",
      simd == 3 ~ "3",
      simd == 4 ~ "4",
      simd == 5 ~ "5 - Least Deprived"),
    # Convert sex column from integer to string
    sex = case_when(
      sex == "M" ~ "01 Male",
      sex == "F" ~ "02 Female"),
    # Convert age column from string to numeric
    age = as.numeric(gsub("\\D", "", age)),
    age_grp_2 = case_when(
      age %in% 0:79  ~ "79 and Under",
      age %in% 80:84 ~ "80 to 84",
      age >= 85      ~ "85+")   
  ) %>%
  # Put health board and ijb in the same column named geog
  pivot_longer(c(scot, ijb, health_board), values_to = "geog")

# Add missing years until the current year by duplicating the latest year in population_df
while(max(population_df$fy) < fy){
  population_df %<>% 
    rbind((population_df %>% 
             filter(fy == max(fy)) %>% 
             mutate(fy = max(fy) + 1)))
  message("Population data from ", max(population_df$fy), " duplicated for ", max(population_df$fy) + 1)
}

# Convert fy column from YYYY to "YYYY/YY"
population_df <- population_df %>%
  mutate(fy = paste0(fy, "/", sprintf("%02d", (fy + 1) %% 100)))

################################################################################.
### 4. Save data ----
################################################################################.

if (exists("save_output") && isTRUE(save_output)){
  population_df %>% 
    write_rds(paste0(output_path, "error.rds"))
}
################################ END OF SCRIPT #################################.