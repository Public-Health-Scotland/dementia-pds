#########################################################################
# Name of file - 02_calculate-further-measures.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Abram McCormick
# Original Date - January 2025

# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - creates data frames for use in management reports.
#########################################################################


### 0 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))

### 1 load functions ----
source(here::here("functions", "summarise_functions.R"))


#read in individual data ----
ldp <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) %>% 
  
  mutate(ldp = word(ldp, 1)) %>% 
  
    # Remove codes from board, IJB, and sex
  mutate(n_referrals = 1,
         health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

# remove codes
variables <- c("ethnic_group",
               "additional_disability",
               "living_alone",
               "accommodation_type",
               "subtype_of_dementia",
               "clinical_impression_of_stage_of_illness",
               "model_of_care",
               "pds_referral_source",
               "pds_uptake_decision",
               "pds_status",
               "carers_support")

ldp %<>% mutate(across(all_of(variables), ~substring(.x, 3))) %>%
  mutate(across(all_of(variables), ~if_else(is.na(.x), "Not Known", .x))) %>%
  mutate(across(all_of(variables), ~str_trim(.x, "left")))

 ldp %<>% mutate(sex = substring(sex, 3)) %>% 
   mutate(sex = str_trim(sex, "left")) %>% 
  mutate(sex = if_else(is.na(sex) | sex == "Not Known", "Unknown", sex)) 


ldp %<>% mutate(accommodation_type = if_else(accommodation_type %in% c("Not Known", "Homeless", "No Fixed Address"),
                                             "Not Known/Other", accommodation_type),
                pds_referral_source = if_else(pds_referral_source %in% c("Not Known", "Local Authority", "Other", "Private Professional/Service/Organisation", "Self Referral"),
                                              "Not Known/Other", pds_referral_source))
### PATHWAYS ----

ldp_wait_times <- ldp %>% 
  mutate(n_referrals = 1,
         diagnosis_to_referral_days = time_length(interval(dementia_diagnosis_confirmed_date, date_pds_referral_received), "days"),
         referral_to_allocation_days = time_length(interval(date_pds_referral_received, initial_pds_practitioner_allocation_date), "days"),
         allocation_to_contact_days = time_length(interval(initial_pds_practitioner_allocation_date, date_of_initial_first_contact), "days"),
         referral_to_contact_days = time_length(interval(date_pds_referral_received, date_of_initial_first_contact), "days"),
         diagnosis_to_contact_days = time_length(interval(dementia_diagnosis_confirmed_date, date_of_initial_first_contact), "days"),
         contact_to_termination_days = time_length(interval(date_of_initial_first_contact, termination_or_transition_date), "days"),
         referral_to_termination_days = time_length(interval(date_pds_referral_received, termination_or_transition_date), "days")
  )

ldp_wait_times %<>%
  #   mutate(termination_or_transition_reason = if_else(ldp == 'exempt', paste0(termination_or_transition_reason, " (exempt from LDP Standard)"), termination_or_transition_reason)) %>%
  #  mutate(termination_or_transition_reason = substring(termination_or_transition_reason, 3)) %>% 
  mutate(termination_or_transition_reason = if_else(is.na(termination_or_transition_date), "PDS Active", termination_or_transition_reason)) %>%
  mutate(termination_or_transition_reason = if_else(is.na(termination_or_transition_reason), "13 Unknown Reason", termination_or_transition_reason))
# mutate(termination_or_transition_reason = str_trim(termination_or_transition_reason, "left"))

ldp_wait_times %>% 
  write_file(path = get_mi_data_path("ldp_wait_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.


# create summary
data_wait <- summarise_pathways(ldp_wait_times)

data_wait %<>% mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb),
                      perc_allocated = round((allocated_referrals/total_referrals)*100,1),
                      perc_contacted = round((contacted_referrals/total_referrals)*100,1))


data_wait_2 <- summarise_pathways_2(ldp_wait_times)

data_wait_2 %<>% mutate(ijb = if_else(ijb == "All", health_board, ijb))

data_wait_3 <- summarise_pathways_3(ldp_wait_times)

data_wait_3 %<>% mutate(ijb = if_else(ijb == "All", health_board, ijb))


data_wait %>% 
  write_file(path = get_mi_data_path("wait_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

data_wait_2 %>% 
  write_file(path = get_mi_data_path("wait_data_2", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

data_wait_3 %>% 
  write_file(path = get_mi_data_path("wait_data_3", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.


### DATA COMPLETION ----

ldp_select <- ldp %>% select(-contact_before_diag, -pds_calc_months, -date2, -ddcd_check,
                                                  -fake_id, -simd, -age, -age_grp, -ldp, -pds_11, -pds_12,
                                                  -diag_12, -month, -x29, -x30, -error_flag)

# calculate yet to be determined totals

set_ytbd <- function(x){
  case_when(str_detect(x, "Yet to be determined") ~ 1,
                TRUE ~ 0)
}

cols <- names(ldp_select %>% select(-fy,-health_board,-ijb))


ldp_select_ytbd <- ldp_select %>%  mutate(across(all_of(cols), set_ytbd))


totals_hb <- bind_rows(
  ldp_select_ytbd %>% group_by(fy, health_board) %>% summarise(number_of_records = n()),
  
  ldp_select_ytbd %>% group_by(fy, health_board = "Scotland") %>% summarise(number_of_records = n()),
  
  ldp_select_ytbd %>% group_by(fy = "All", health_board) %>% summarise(number_of_records = n()),
  
  ldp_select_ytbd %>% group_by(fy = "All", health_board = "Scotland") %>% summarise(number_of_records = n())
  
  )



ytbd_hb <- bind_rows(
  
  ldp_select_ytbd %>% group_by(fy, health_board) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_ytbd %>% group_by(fy, health_board = "Scotland") %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_ytbd %>% group_by(fy = "All", health_board) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_ytbd %>% group_by(fy = "All", health_board = "Scotland") %>% summarise(across(all_of(cols), sum, .names = "{.col}"))
  )


hb <- full_join(ytbd_hb, totals_hb) %>% rename(geog = health_board)



totals_ijb <- bind_rows(
  ldp_select_ytbd %>% group_by(fy, ijb) %>% summarise(number_of_records = n()),
  
  ldp_select_ytbd %>% group_by(fy = "All", ijb) %>% summarise(number_of_records = n())
)
  

ytbd_ijb <- bind_rows(
  ldp_select_ytbd %>% group_by(fy, ijb) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_ytbd %>% group_by(fy = "All", ijb) %>% summarise(across(all_of(cols), sum, .names = "{.col}"))
)


ijb <- full_join(ytbd_ijb, totals_ijb) %>% rename(geog = ijb)

summary_ytbd <- bind_rows(ijb,hb)  


summary_ytbd %<>% pivot_longer(cols= cols,
                          names_to='field_name',
                          values_to='no_of_records_ytbd') %>% 
  relocate(number_of_records, .after = field_name)




# calculate na and unknown totals

set_na <- function(x){
  case_when(str_detect(x, "^99") ~ NA,
            str_detect(x, "^98") ~ NA,
            str_detect(x, "^N/A")  ~ NA,
            str_detect(x, "^unknown")  ~ NA,
            str_detect(x, "^Unknown")  ~ NA,
            TRUE ~ x)
}


ldp_select_na <- ldp_select %>%  mutate(across(all_of(cols), set_na))

na_fun <- function(x){  
  if_else(is.na(x),1,0)
}

ldp_select_na %<>% mutate(across(all_of(cols), na_fun))


missing_hb <- bind_rows(
  
  ldp_select_na %>% group_by(fy, health_board) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_na %>% group_by(fy, health_board = "Scotland") %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_na %>% group_by(fy = "All", health_board) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_na %>% group_by(fy = "All", health_board = "Scotland") %>% summarise(across(all_of(cols), sum, .names = "{.col}"))
  )


missing_hb %<>% rename(geog = health_board)


missing_ijb <-bind_rows(
  ldp_select_na %>% group_by(fy, ijb) %>% summarise(across(all_of(cols), sum, .names = "{.col}")),
  
  ldp_select_na %>% group_by(fy = "All", ijb) %>% summarise(across(all_of(cols), sum, .names = "{.col}"))
)

missing_ijb %<>% rename(geog = ijb)


summary_na <- bind_rows(missing_ijb, missing_hb)  


summary_na %<>% pivot_longer(cols= cols,
                          names_to='field_name',
                          values_to='no_of_records_missing_not_known')

summary <- full_join(summary_ytbd, summary_na)


summary %<>% mutate(mandatory_optional = 
                      case_when(field_name %in% c("locality", "additional_disability", "living_alone", 
                                                  "subtype_of_dementia", "practitioner_team_id", "carers_support") ~ "optional",
                                field_name %in% c("date_pds_referral_received") ~ "conditional mandatory from 01/04/2019",
                                field_name %in% c("initial_pds_practitioner_allocation_date", "date_of_initial_first_contact",
                                                  "termination_or_transition_date", "termination_or_transition_reason") ~ "conditional mandatory",
                                TRUE ~ "mandatory"), .after = field_name)

summary %<>% mutate(perc_of_records_missing_not_known = round(100*no_of_records_missing_not_known/number_of_records, 1))

summary %<>% mutate(perc_of_records_ytbd = round(100*no_of_records_ytbd/number_of_records, 1))

summary %>% 
   write_file(path = get_mi_data_path("comp_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.
 

#SUBTYPE OF DEMENTIA----
data_subtype <- summarise_by_variable(subtype_of_dementia) %>% 
  mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb)) %>% 
  mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>% 
  rename(geog = ijb, subtype = type)

data_subtype %>% 
  write_file(path = get_mi_data_path("subtype_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

#STAGE OF ILLNESS----

data_stage <- summarise_by_variable(clinical_impression_of_stage_of_illness) %>% 
  mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb)) %>% 
  mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>% 
  rename(geog = ijb, stage = type)

data_stage %>% 
  write_file(path = get_mi_data_path("stage_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

#MODEL OF CARE----

data_model <- summarise_by_variable_2(model_of_care) %>% 
  mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb)) %>% 
  mutate(ijb = if_else(ijb == "All", health_board, ijb)) %>% 
  rename(geog = ijb, model = type)

data_model %>% 
  write_file(path = get_mi_data_path("model_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

#DATA UPTAKE----

data_uptake <- summarise_uptake(ldp)

data_uptake %>% 
  write_file(path = get_mi_data_path("uptake_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

#DATA UPTAKE----

data_carer <- summarise_uptake(ldp, field = carers_support)

data_carer %>% 
  write_file(path = get_mi_data_path("carer_data", ext = "rds", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

### END OF SCRIPT ###

