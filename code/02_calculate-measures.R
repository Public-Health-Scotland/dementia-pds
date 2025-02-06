#########################################################################
# Name of file - 02_calculate-measures.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
# Updated by - Jennifer Thom
# Date - November 2023
# Updated by Abram McCormick - January 2025
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Add LDP Standard classification and other measures to be
# included in reports.
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Load data ----

pds <- read_rds(get_mi_data_path("clean_data", ext = "rds", test_output = test_output))
  

### 3 - Add FY and months labels ----

pds %<>%
  mutate(fy    = extract_fin_year(dementia_diagnosis_confirmed_date),
         month = month(dementia_diagnosis_confirmed_date))


### 4 - Add key dates for calculations ----

pds %<>%
  
  mutate(
    
    # Date 12 months after diagnosis date
    diag_12 = add_with_rollback(dementia_diagnosis_confirmed_date, 
                                months(12),
                                roll_to_first = TRUE),
    
    # Date 11 months after date of first PDS contact     
    pds_11 = add_with_rollback(date_of_initial_first_contact, 
                               months(11),
                               roll_to_first = TRUE),
    
    # Date 12 months after date of first PDS contact
    pds_12 = add_with_rollback(date_of_initial_first_contact, 
                               months(12),
                               roll_to_first = TRUE)
    
  )

pds %<>%
  
  mutate(contact_before_diag = if_else(
    date_of_initial_first_contact < dementia_diagnosis_confirmed_date, 1, 0)
  )


### 5 - Add LDP standard classification ----

pds %<>%
  
  mutate(ldp = case_when(
    
    #### COMPLETE ####
    
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    date_of_initial_first_contact < diag_12 & 
      end_date >= pds_12 &
      is.na(termination_or_transition_date)
    ~ "complete - Without termination date",
    
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    date_of_initial_first_contact < diag_12 &
      termination_or_transition_date >= pds_11
    ~ "complete - PDS ended",
    
    #### FAIL ####
    
    # PDS started more than 12m after diagnosis
    date_of_initial_first_contact >= diag_12
    ~ "fail - PDS started more than 12 months after diagnosis",
    
    # More than 12m since diagnosis and PDS not started
    end_date >= diag_12 & 
      is.na(date_of_initial_first_contact) &
      is.na(termination_or_transition_date)
    ~ "fail - PDS not started and more than 12 months since diagnosis",
    
    # PDS terminated before 11 months from start date
    termination_or_transition_date < pds_11 &
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail - PDS terminated less than 11 months after first contact",
    
    # PDS terminated before first contact made
    is.na(date_of_initial_first_contact) & 
      !is.na(termination_or_transition_date) & 
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail - PDS terminated before first contact",
    
    #### EXEMPT ####
    
    # Exempt termination reason; died
    substr(termination_or_transition_reason, 1, 2) == "03"
    ~ "exempt - 03 Service user has died",
    
    # Exempt termination reason; moved to other HB
    substr(termination_or_transition_reason, 1, 2) == "04"
    ~ "exempt - 04 Service user has moved to a different Health Board area",
    
    # Exempt termination reason; refused
    substr(termination_or_transition_reason, 1, 2) == "05"
    ~ "exempt - 05 Service user has terminated PDS early/refused",
    
    # Exempt termination reason; can't engage
    substr(termination_or_transition_reason, 1, 2) == "06"
    ~ "exempt - 06 Service user no longer able to engage in PDS",
    
    #### ONGOING ####
    
    # Less than 12m since diagnosis and PDS not started
    end_date < diag_12 & 
      is.na(date_of_initial_first_contact) & 
      is.na(termination_or_transition_date)
    ~ "ongoing - PDS not started and less than 12 months since diagnosis",
    
    # PDS started within 12m of diagnosis but not yet ended
    date_of_initial_first_contact < diag_12 &
      end_date < pds_12 &
      is.na(termination_or_transition_date)
    ~ "ongoing - Still receiving PDS and less than 12 months since first contact"
    
  ))


### 6 - Add Age Group and Deprivation ----

pds %<>%
  
  mutate(age = 
           floor(time_length(
             interval(date_of_birth,
                      dementia_diagnosis_confirmed_date),
             "years"))
  ) %>%
  mutate(age_grp = 
           case_when(
             age <= 0 | is.na(age) ~ "Unknown",
             age %in% 1:59 ~ "59 and Under",
             age %in% 60:64 ~ "60 to 64",
             age %in% 65:69 ~ "65 to 69",
             age %in% 70:74 ~ "70 to 74",
             age %in% 75:79 ~ "75 to 79",
             age %in% 80:84 ~ "80 to 84",
             age %in% 85:89 ~ "85 to 89",
             age >= 90      ~ "90+"
           )) %>%
  
  # updated to include broad age groups
  mutate(age_grp_2 = 
           case_when(
             age < 0 | is.na(age) ~ "Unknown",
             age %in% 1:64 ~ "64 and Under",
             age >= 65      ~ "65+"
           )) %>%
  
  mutate(postcode = format_postcode(postcode)) %>%
  left_join(simd(), by = c("postcode" = "pc7")) %>%
  mutate(simd = replace_na(simd, "Unknown"))
          

### 7 - Save individual level file for checking ----
pds %>% 
write_file(path = get_mi_data_path("ldp_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.
             
pds %>% 
write_file(path = get_mi_data_path("ldp_data", ext = "csv", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.


### 8 - Create final output file ----

inc_months <-
  if(qt == 1){4:6}else{
    if(qt == 2){4:9}else{
      if(qt == 3){4:12}else{
        if(qt == 4){1:12}
      }
    }
  }

pds %<>%
  
  # Aggregate to create minimal tidy dataset
  group_by(health_board, ijb, fy, month, age_grp_2, age_grp, simd, sex, ldp) %>% #updated to include age groups, gender and simd
  summarise(referrals = n(), .groups = "drop") %>%
  

  # Add rows where no referrals were made
  # Doing this will make sure zeros are still shown in reports
  complete(nesting(health_board, ijb), fy, month, ldp,
           fill = list(referrals = 0,
                       age_grp = "Unknown",
                       simd = "Unknown",
                       age_grp_2 = "Unknown",
                       sex = "Unknown")) %>%
  
  # Remove LDP reason detail
  rename(ldp_full = ldp) %>% 
  mutate(ldp = word(ldp_full, 1), .after = ldp_full) %>% 

  # Remove completed rows for months in incomplete financial year
  # e.g. for Q1 reports, remove completed rows for July - March
  filter(substr(fy, 1, 4) < year(end_date) |
           (substr(fy, 1, 4) == year(end_date) & 
              month %in% inc_months))
  
# write final data
pds %>% 
write_file(path = get_mi_data_path("final_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.


### END OF SCRIPT ###