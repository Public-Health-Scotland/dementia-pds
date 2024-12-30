# Load PDS data
pds <- read_rds(get_mi_data_path("final_data", ext = "rds", test_output = test_output)) %>% 
  
  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

pds_ijb <- pds %>% arrange(ijb)

pds_scot <- pds %>% group_by(health_board = "Scotland", ijb = "Scotland", fy, month, ldp, age_grp, simd) %>% 
  summarise(referrals = sum(referrals), .groups = "drop")

pds_hb <- pds %>% group_by(health_board, fy, month, ldp, age_grp, simd) %>% 
  summarise(referrals = sum(referrals), .groups = "drop") %>% mutate(ijb = health_board, .before = fy)

pds_all <- bind_rows(pds_scot, pds_hb, pds_ijb)

pds_all %<>% rename(geog = ijb) 

pds_all$geog<- factor(pds_all$geog, levels = unique(pds_all$geog))

#read in individual data
ldp <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) %>% 
  
  mutate(ldp = word(ldp, 1)) %>% 
  
    # Remove codes from board, IJB, and sex
  mutate(n_referrals = 1,
         health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

# # remove codes
# variables <- c("ethnic_group",  
#                "additional_disability",
#                "living_alone",
#                "accommodation_type",
#                "subtype_of_dementia",
#                "clinical_impression_of_stage_of_illness",
#                "model_of_care",
#                "pds_referral_source",
#                "pds_uptake_decision",
#                "pds_status",
#                "carers_support")

# ldp %<>% mutate(across(all_of(variables), ~substring(.x, 3))) %>% 
#   mutate(across(all_of(variables), ~if_else(is.na(.x), "Not Known", .x))) %>% 
#   mutate(across(all_of(variables), ~str_trim(.x, "left"))) 
# 
 ldp %<>% mutate(sex = substring(sex, 3)) %>% 
   mutate(sex = str_trim(sex, "left")) %>% 
  mutate(sex = if_else(is.na(sex) | sex == "Not Known", "Unknown", sex)) 
# 
# 
# ldp %<>% mutate(accommodation_type = if_else(accommodation_type %in% c("Not Known", "Homeless", "No Fixed Address"),
#                                              "Not Known/Other", accommodation_type),
#                 pds_referral_source = if_else(pds_referral_source %in% c("Not Known", "Local Authority", "Other", "Private Professional/Service/Organisation", "Self Referral"),
#                                               "Not Known/Other", pds_referral_source))


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


### END OF SCRIPT ###

