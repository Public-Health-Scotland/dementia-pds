################################################################################.
# Name of file - 01_clean-pds-data.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
# Updated by - Jennifer Thom
# Date - November 2023
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - 
#   Clean collated file provided by Data Management team 
#   Add data from finalised years
#   Create query and error reports
#   Remove duplicates
#   NOTE: save_output must be set to TRUE for the output to be saved.
################################################################################.

################################################################################.
### 1 - Load data ----
################################################################################.

# Environment file
source(here::here("code", "00_setup-environment.R"))

################################################################################.
### 2 - Read and clean latest submission ----
################################################################################.

# Collated file for latest submission (output from Data Management)
pds <- readr::read_csv(get_national_data_path(), col_types = cols(.default = "c")) %>%
  
  # Convert column names to snake case
  janitor::clean_names() %>%
  
  # Convert dates from character to date format
  dplyr::mutate(dplyr::across(tidyselect::contains("date"), lubridate::ymd)) %>%
  
  # Pad CHI Number to 10 digits
  dplyr::mutate(chi_number = phsmethods::chi_pad(chi_number)) %>%
  
  # Replace word 'and' with ampersand
  dplyr::mutate(health_board = stringr::str_replace(health_board, " and ", " & ")) %>%
  
  # Remove records with missing diagnosis date or outwith reporting period
  dplyr::filter(dplyr::between(dementia_diagnosis_confirmed_date, start_date, end_date))
  
################################################################################.
### 2 - Save newly finalised data (Q4 non-test output only) ----
################################################################################.

if (qt == 4 && test_output == FALSE){
  
  # Name of the new final file
  new_final_file <- paste0(get_final_data_dir(), "/", str_replace(revised_year, '/', '-'), "_final-data.rds")
  
  # Stop if the file already exists
  if(file.exists(new_final_file)){
    stop(paste0("A finalised data file for ", revised_year, " already exists at ", new_final_file, "."))
  
  # Save if the file does not already exist
  } else {
    
    # Get data for newly finalised years in latest submission
    new_finalised_data <- pds %>%
      dplyr::filter(extract_fin_year(dementia_diagnosis_confirmed_date) == revised_year)
    
    # Save final file
    readr::write_rds(new_finalised_data, new_final_file, compress = "gz")
    
    # Print message saying the file has been saved
    message(paste0("File saved to ", new_final_file))
  }
}

################################################################################.
### 3 - Add saved finalised data ----
################################################################################.

# Read in and bind together data for finalised years if available
finalised_data <- data.frame()
for (year in finalised_years){
  final_file <- paste0(get_final_data_dir(), "/", str_replace(year, '/', '-'), "_final-data.rds")
  if (file.exists(final_file)){
    finalised_data <- bind_rows(finalised_data, read_rds(final_file))
  } else {
    message(paste0("Final data does not yet exist for ", year, "."))
  }
}
    
# List all years included in the finalised data
finalised_data_years <- unique(extract_fin_year(finalised_data$dementia_diagnosis_confirmed_date))

# Remove years included in the finalised data from the current submission
pds <- pds %>% 
  dplyr::filter(!extract_fin_year(dementia_diagnosis_confirmed_date) %in% finalised_data_years)
  
# Add finalised data to current submission
pds <- bind_rows(finalised_data, pds)

################################################################################.
### 4 - Create error and query summaries ----
################################################################################.

# Queries and errors for all years
q_err <- pds %>%
  
  # Remove health board code and replace NA with "Missing"
  mutate(
    health_board = if_else(
      is.na(health_board),
      "Missing",
      substring(health_board, 3))) %>%
  
  # Create financial year column based on diagnosis date
  mutate(fy = extract_fin_year(dementia_diagnosis_confirmed_date)) %>%
  
  # Group by financial year, health board and IAA then count total number of query/error flags and total number of records
  group_by(fy, health_board, ijb) %>%
  summarise(
    total_q_errors = sum(as.integer(error_flag)),
    total_queries  = sum(as.integer(q_flag)),
    total_errors   = sum(as.integer(e_flag)),
    records        = n(),
    .groups = "drop") %>%
  
  # Add rows for the total number of query/error flags and total number of records for each financial year over all health boards and IAAs
  group_by(fy) %>%
  group_modify(~ bind_rows(.x,summarise(
    .x,
    health_board = "Scotland",
    ijb = "Scotland",
    across(
      c(total_q_errors, total_queries, total_errors, records),
      \(x) sum(x, na.rm = TRUE))))) %>%
  ungroup() %>%
  
  # Order by financial year, health board and IAA
  arrange(fy, health_board, ijb)

################################################################################.
### 5 - Apply any ad hoc changes as discussed ----
################################################################################.

pds <- pds %>%
  
  # Change health board for North/South Lanarkshire to "L NHS Lanarkshire" (from "G NHS Greater Glasgow & Clyde")
  mutate(health_board = case_when(
    str_detect(ijb, "S37000035|S37000028") ~ "L NHS Lanarkshire",
    TRUE ~ health_board)) %>%
  
  # Remove rows with unknown IAA
  filter(!is.na(ijb))

# Remove chi numbers where health board does not match IAA
chi_to_remove <- (pds %>% filter(health_board == "V NHS Forth Valley", ijb == "S37000033 Perth and Kinross"))$chi_number
pds <- pds %>% filter(!chi_number %in% chi_to_remove)

################################################################################.
### 6 - Flag duplicate records ----
################################################################################.

pds <- pds %>%
  
  # Group by CHI number
  group_by(chi_number) %>%
  
  # Add flag where CHI number is duplicated
  mutate(dupe = as.integer(!is.na(chi_number) & n() > 1)) %>%
  
  # Add flag where CHI number is duplicated and there is at least one record from NHS Greater Glasgow & Clyde and at least one record from NHS Highland
  mutate(ggc_h_dupe = as.integer(
    dupe == 1 &
      all(c("G NHS Greater Glasgow & Clyde", "H NHS Highland") %in% health_board))) %>%
  
  # Add flag where CHI number is duplicated there is at least one record that has moved to a different health board and at least one record that has not
  mutate(termination_dupe = as.integer(
    dupe == 1 &
      n_distinct(termination_or_transition_reason) > 1 &
      any(str_detect(termination_or_transition_reason, "^04")))) %>%
  
  # Add priority flag for duplicates to keep
  mutate(dupe_keep = 
           case_when(
             
             # 1. Where there is at least one record from NHS Greater Glasgow & Clyde and at least one record from NHS Highland:
             # Keep the record(s) from NHS Greater Glasgow & Clyde
             ggc_h_dupe == 1 & health_board == "G NHS Greater Glasgow & Clyde" ~ 1,
             # Remove the record(s) from NHS Highland
             ggc_h_dupe == 1 & health_board == "H NHS Highland" ~ 0,
             
             # 2. Where at least 2 records have different diagnosis dates:
             # Keep the record(s) with the earliest diagnosis date 
             dupe == 1 &
               n_distinct(dementia_diagnosis_confirmed_date) > 1 &
               dementia_diagnosis_confirmed_date == min(dementia_diagnosis_confirmed_date, na.rm = TRUE) ~ 1,
             # Remove the record(s) with other diagnosis dates or NA
             dupe == 1 &
               n_distinct(dementia_diagnosis_confirmed_date) > 1 &
               (!(dementia_diagnosis_confirmed_date == min(dementia_diagnosis_confirmed_date, na.rm = TRUE)) |
                  is.na(dementia_diagnosis_confirmed_date)) ~ 0,
             
             # 3. Where there is at least one record that has moved to a different health board and at least one record that has not: 
             # Keep the record(s) that moved health board
             dupe == 1 &
               n_distinct(termination_or_transition_reason) > 1 &
               str_detect(termination_or_transition_reason, "^04") ~ 1,
             # Remove the record(s) which terminated/transitioned for other reasons or NA
             dupe == 1 &
               n_distinct(termination_or_transition_reason) > 1 &
               any(str_detect(termination_or_transition_reason, "^04")) ~ 0,
             
             # 4. Where at least 2 records have different contact dates:
             # Keep the record(s) with the earliest contact date 
             dupe == 1 &
               n_distinct(date_of_initial_first_contact) > 1 &
               date_of_initial_first_contact == 
               min(date_of_initial_first_contact, na.rm = TRUE) ~ 1,
             # Remove the record(s) with other dates or NA
             dupe == 1 &
               n_distinct(date_of_initial_first_contact) > 1 &
               (!(date_of_initial_first_contact == min(date_of_initial_first_contact, na.rm = TRUE)) |
                  is.na(date_of_initial_first_contact)) ~ 0,
             
             # Keep all records that aren't duplicates
             dupe != 1 ~ 1)) %>%
  
  ungroup()

# Check that all duplicate records have been removed
remaining_duplicates <- pds %>% filter(dupe_keep == 1) %>% group_by(chi_number) %>% filter(!is.na(chi_number) & n() > 1)
if(nrow(remaining_duplicates > 0)){
  message(paste0("There are ", nrow(remaining_duplicates), "duplicate records remaining. Please check the data manually."))
} else {
  message("All duplicates removed successfully.")
}

################################################################################.
### 7 - Remove duplicate records ----
################################################################################.

pds <- pds %>%
  
  # Remove duplicates that are not flagged to keep
  filter(dupe_keep == 1) %>%
  
  # Change health board for Glasgow/Highland duplicates to "H NHS Highland" (from "G NHS Greater Glasgow & Clyde") and IAA to "S37000004 Argyll and Bute"
  mutate(health_board = 
           if_else(ggc_h_dupe == 1, "H NHS Highland", health_board),
         ijb = if_else(ggc_h_dupe == 1, "S37000004 Argyll and Bute", ijb)) %>%
  
  # Remove dupe helper columns
  select(-contains("dupe"))

################################################################################.
### 8 - Save data ----
################################################################################.

if (exists("save_output") && isTRUE(save_output)){
  
  # Save queries + errors for all years
  q_err %>% 
    select(-total_queries, -total_errors) %>%
    write_file(path = get_mi_data_path("q_error_data", ext = "rds", test_output = test_output))
  0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.
  
  # Save queries for 2021/22 onward
  q_err %>% 
    filter(as.integer(substr(fy, 1, 4)) >= 2021) %>% 
    select(-total_q_errors, -total_errors) %>%
    write_file(path = get_mi_data_path("query_data", ext = "rds", test_output = test_output))
  0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.
  
  # Save errors for 2021/22 onward
  q_err %>% 
    filter(as.integer(substr(fy, 1, 4)) >= 2021) %>% 
    select(-total_q_errors, -total_queries) %>%
    write_file(path = get_mi_data_path("error_data", ext = "rds", test_output = test_output))
  0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

  # Save duplicate records
  dupes <- 
    pds %>% 
    filter(dupe == 1) %T>%
    write_file(path = get_mi_data_path("dupe_data", ext = "csv", test_output = test_output))
  0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.
  
  # Save cleaned PDS data
  pds %>% 
    write_file(path = get_mi_data_path("clean_data", ext = "rds", test_output = test_output))
  0 # This zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

}

################################ END OF SCRIPT #################################.