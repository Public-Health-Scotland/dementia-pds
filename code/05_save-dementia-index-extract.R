################################################################################.
# Name of file - 05_save-dementia-index-extract.R
# Original Authors - Lucy Binsted
# Original Date - September 2026
#
# Written/run on - R Posit
# Version of R - 4.4.2
#
# Description - Save PDS data to send to Dementia Index Team.
################################################################################.

################################################################################.
### 0 - Load environment file ----
################################################################################.

source(here::here("code", "00_setup-environment.R"))

################################################################################.
### 1 - Read data and select columns ----
################################################################################.

# Read cleaned data (output from 01_data-preparation.R)
pds_dementia_index <- read_rds(get_mi_data_path(
  type = "clean_data", 
  ext = "rds", 
  fy = fy,
  qt = qt,
  test_output = FALSE)) %>% 
  # Select required columns
  select(
    chi_number, date_of_birth, sex, ethnic_group, postcode, 
    dementia_diagnosis_confirmed_date, subtype_of_dementia, health_board) %>%
  mutate(
    # Remove health board codes
    health_board = str_sub(health_board, 3, -1),
    # Format postcode
    postcode = format_postcode(postcode))

################################################################################.
### 2 - Save data ----
################################################################################.

# Year in yyyy_yy format
year <- paste0(fy, "_", substr(as.character(as.numeric(fy) + 1), 3, 4))

# Dementia Index Extract Folder
filepath <- "/conf/dementia/A&I/IR-PQ-FOI/IR2025-00094 Dementia Index Extract/output/"

# Dementia Index Extract File name
filename <- paste0("pds_dementia_index_extract-", year, "-Q", qt)

# Write as .rds
write_rds(pds_dementia_index, paste0(filepath, filename, ".rds"))

# Write as .csv
write.csv(pds_dementia_index, paste0(filepath, filename, ".csv"), row.names=FALSE)
