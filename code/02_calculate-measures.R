#########################################################################
# Name of file - 02_calculate-measures.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Add LDP Standard classification and other measures to be
# included in reports.
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Load data ----

pds <- read_rds(mi_data_path("clean_data", "rds"))
  

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


### 5 - Add LDP standard classification ----

pds %<>%
  
  mutate(ldp = case_when(
    
    #### COMPLETE ####
    
    # Started PDS within 12m of diagnosis AND PDS still ongoing after 12m
    date_of_initial_first_contact < diag_12 & 
      end_date >= pds_12 &
      is.na(termination_or_transition_date)
    ~ "complete - still receiving",
    
    # Started PDS within 12m of diagnosis AND PDS ended after 11m
    date_of_initial_first_contact < diag_12 &
      termination_or_transition_date >= pds_11
    ~ "complete - ended",
    
    #### FAIL ####
    
    # PDS started more than 12m after diagnosis
    date_of_initial_first_contact >= diag_12
    ~ "fail - started >12m after diag",
    
    # More than 12m since diagnosis and PDS not started
    end_date >= diag_12 & 
      is.na(date_of_initial_first_contact) &
      is.na(termination_or_transition_date)
    ~ "fail - not started and >12m since diagnosis",
    
    # PDS terminated before 11 months from start date
    termination_or_transition_date < pds_11 &
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail - term <11m from first contact",
    
    # PDS terminated before first contact made
    is.na(date_of_initial_first_contact) & 
      !is.na(termination_or_transition_date) & 
      !(substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons)
    ~ "fail - term before first contact",
    
    #### EXEMPT ####
    
    # Exempt termination reason; died/moved to other HB/refused/can't engage
    substr(termination_or_transition_reason, 1, 2) %in% exempt_reasons
    ~ "exempt",
    
    #### ONGOING ####
    
    # Less than 12m since diagnosis and PDS not started
    end_date < diag_12 & 
      is.na(date_of_initial_first_contact) & 
      is.na(termination_or_transition_date)
    ~ "ongoing - not started",
    
    # PDS started within 12m of diagnosis but not yet ended
    date_of_initial_first_contact < diag_12 &
      end_date < pds_12 &
      is.na(termination_or_transition_date)
    ~ "ongoing - still receiving"
    
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
  
  mutate(postcode = format_postcode(postcode)) %>%
  left_join(simd(), by = c("postcode" = "pc7")) %>%
  mutate(simd = replace_na(simd, "Unknown"))
          

### 7 - Save individual level file for checking ----
pds %>% 
write_rds(mi_data_path("ldp_data", "rds"), compress = "gz")

pds %>% 
write_csv(mi_data_path("ldp_data", "csv"))


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
  group_by(health_board, ijb, fy, month, ldp, age_grp, simd) %>%
  summarise(referrals = n(), .groups = "drop") %>%
  
  # Add rows where no referrals were made
  # Doing this will make sure zeros are still shown in reports
  complete(nesting(health_board, ijb), fy, month,
           fill = list(referrals = 0,
                       ldp = "complete",
                       age_grp = "Unknown",
                       simd = "Unknown")) %>%

  # Remove completed rows for months in incomplete financial year
  # e.g. for Q1 reports, remove completed rows for July - March
  filter(substr(fy, 1, 4) < year(end_date) |
           (substr(fy, 1, 4) == year(end_date) & 
              month %in% inc_months)) %>%
  
  # Remove LDP reason detail
  mutate(ldp = word(ldp, 1))

# write final data
pds %>% 
write_rds(mi_data_path("final_data", "rds"), compress = "gz")

### END OF SCRIPT ###