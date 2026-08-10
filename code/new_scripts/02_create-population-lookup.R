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

# Helper function to get the geography year e.g. HSCP2019, HB2019
# Enables dynamic column selection
get_pop_est_year <- function(path) {
  stringr::str_match(
    fs::path_file(path),
    "^[^0-9]+([0-9]{4})_"
  )[, 2]
}

################################################################################.
### 2. Read latest SIMD population file ----
################################################################################.

simd_pop <- read_rds(
  get_pop_path(
    type = "DataZone", 
    selection_method = "modification_date"
  )
)

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