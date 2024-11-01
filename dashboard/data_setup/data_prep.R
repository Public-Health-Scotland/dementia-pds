################################################################################
# Name of file - data_prep_1.R
# Original Authors - Jennifer Thom
# Original Date - June 2023
# Updated by Abram McCormick - Sep 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - script 1 of 2 to prepare PDS data for use in R shiny. 
################################################################################

# 1 Load set up ----
source(here::here("dashboard", "00_setup-environment.R"))

provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
       "/", as.numeric(substr(last(finalised_years),6,7)) + 1)

#included_years = c(finalised_years, provisional_year)

included_years <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

# 2 Load PDS data ----
pds <- read_rds(get_mi_data_path("final_data", ext = "rds", test_output = test_output)) %>% 
  
  # inlcude only finalised years
  filter(fy %in% included_years) %>% 
  
  
  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))

boards <- sort(unique(pds$health_board))

# 3 prepare data for plot_referrals() ----

pds_plot_data <- pds %>%  
  group_by(fy, month, health_board, ijb) %>%
  summarise(referrals = sum(referrals), .groups = "drop") %>% 
  arrange(ijb)

board <- 
  pds %>%
  mutate(ijb = health_board) %>%
  group_by(fy, month, health_board, ijb) %>%
  summarise(referrals = sum(referrals), .groups = "drop")

pds_plot_data <-
  bind_rows(board, pds_plot_data) %>% 
  ungroup() 



 #%>%
 # mutate(ijb = forcats::fct_relevel(ijb, max(.$health_board)))

pds_plot_data %>% 
  write_csv("//conf/dementia/A&I/Outputs/dashboard/data/pds_plot_data.csv")


# 4 Prepare data breakdowns for annual tables ----
prepare_data <- pds %>% 
  # Health board breakdown
  group_by(health_board, ijb = "AAA", fy, ldp) %>%
  summarise(referrals = sum(referrals), .groups = "drop") %>% 
  # Scotland breakdown
  bind_rows(pds %>% 
            group_by(health_board = "Scotland", ijb = "AAA", fy, ldp) %>%
            summarise(referrals = sum(referrals), .groups = "drop")) %>% 
  # IJB breakdown
  bind_rows(pds %>% 
            group_by(health_board, ijb, fy, ldp) %>%
            summarise(referrals = sum(referrals), .groups = "drop")) %>% 
  # Health board breakdown with total
  bind_rows(pds %>% 
  group_by(health_board, ijb = "AAA", fy, ldp = "total") %>%
  summarise(referrals = sum(referrals), .groups = "drop")) %>%
  # Scotland breakdown with total
  bind_rows(pds %>%
  group_by(health_board = "Scotland", ijb = "AAA", fy, ldp = "total") %>%
  summarise(referrals = sum(referrals), .groups = "drop")) %>% 
  # IJB breakdown with total
  bind_rows(pds %>% 
            group_by(health_board, ijb, fy, ldp = "total") %>%
            summarise(referrals = sum(referrals), .groups = "drop")) %>% 
  # Arrange
  arrange(health_board, ijb, fy, ldp) %>% 
  # set values for ongoing as zero
  complete(nesting(health_board, ijb, fy),
           nesting(ldp),
           fill = list(referrals = 0))
  

# 5 calculate rates ----
num_ijb <- pds %>% 
  filter(ldp %in% c("complete", "exempt")) %>% 
  group_by(health_board, ijb, fy) %>% 
  summarise(num = sum(referrals), .groups = "drop") %>%
  bind_rows(pds %>% 
              filter(ldp %in% c("complete", "exempt")) %>% 
              group_by(health_board, ijb = "AAA", fy) %>% 
              summarise(num = sum(referrals), .groups = "drop")) %>% 
  bind_rows(pds %>% 
            filter(ldp %in% c("complete", "exempt")) %>% 
            mutate(health_board = "Scotland", 
                   ijb = "AAA") %>%   
            group_by(health_board, ijb = "AAA", fy) %>% 
            summarise(num = sum(referrals), .groups = "drop")) 


den_ijb <- pds %>% 
  filter(ldp %in% c("complete", "exempt", "fail")) %>% 
  group_by(health_board, ijb, fy) %>% 
  summarise(den = sum(referrals), .groups = "drop") %>%
  bind_rows(pds %>% 
              filter(ldp %in% c("complete", "exempt", "fail")) %>% 
              group_by(health_board, ijb = "AAA", fy) %>% 
              summarise(den = sum(referrals), .groups = "drop")) %>% 
  bind_rows(pds %>% 
              filter(ldp %in% c("complete", "exempt", "fail")) %>% 
              mutate(health_board = "Scotland", 
                     ijb = "AAA") %>% 
              group_by(health_board, ijb = "AAA", fy) %>% 
              summarise(den = sum(referrals), .groups = "drop"))


pds_rate_ijb <- full_join(num_ijb, den_ijb, by = c("health_board", "ijb", "fy")) %>%
  mutate(rate = round(num/den*100, 1),
         rate = replace_na(rate, 0)) %>%
  select(-num, -den)


# Join rate onto prepared data
annual_table_data <- left_join(prepare_data, pds_rate_ijb, by = c("health_board", "ijb", "fy")) %>% 
  arrange(ijb, health_board) %>% 
  mutate(ijb = if_else(ijb == "AAA", health_board, ijb))

# add expected diagnoses data

exp <- read_csv(get_exp_diagnoses_path())

exp %<>% select(-health_board) %>% rename("ijb" = "health_board_label") %>% mutate(ldp = "total")

annual_table_data <- left_join(annual_table_data, exp)

annual_table_data %<>%  mutate(exp_perc = if_else(!is.na(diagnoses), round(referrals/diagnoses*100, 1), NA))


# 6 Save annual output ----
annual_table_data %>% 
write_csv("//conf/dementia/A&I/Outputs/dashboard/data/annual_table_data.csv")


# 7 read in individual data ----
ldp <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) %>% 
  
  filter(fy %in% included_years) %>% 
  
  mutate(ldp = word(ldp, 1)) %>% 

# Remove codes from board, IJB, and sex
mutate(n_referrals = 1,
       health_board = str_sub(health_board, 3, -1),
       ijb          = if_else(is.na(ijb),
                              "Unknown",
                              str_sub(ijb, 11, -1)))
  
# remove codes
variables <- c("ethnic_group",  
               "sex",
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
  
  
ldp %<>% mutate(accommodation_type = if_else(accommodation_type %in% c("Not Known", "Homeless", "No Fixed Address"),
                                             "Not Known/Other", accommodation_type),
                pds_referral_source = if_else(pds_referral_source %in% c("Not Known", "Local Authority", "Other", "Private Professional/Service/Organisation", "Self Referral"),
                                              "Not Known/Other", pds_referral_source))
  

# 8 calculate waiting times ----
ldp_wait_times <- ldp %>% 
  mutate(n_referrals = 1,
         diagnosis_to_referral_days = time_length(interval(dementia_diagnosis_confirmed_date, date_pds_referral_received), "days"),
         referral_to_allocation_days = time_length(interval(date_pds_referral_received, initial_pds_practitioner_allocation_date), "days"),
         allocation_to_contact_days = time_length(interval(initial_pds_practitioner_allocation_date, date_of_initial_first_contact), "days"),
         referral_to_contact_days = time_length(interval(date_pds_referral_received, date_of_initial_first_contact), "days"),
         diagnosis_to_contact_days = time_length(interval(dementia_diagnosis_confirmed_date, date_of_initial_first_contact), "days"),
         contact_to_termination_days = time_length(interval(date_of_initial_first_contact, termination_or_transition_date), "days")
               )


ldp_wait_times %<>%
  mutate(termination_or_transition_reason = if_else(ldp == 'exempt', paste0(termination_or_transition_reason, " (exempt from LDP Standard)"), termination_or_transition_reason)) %>%
  mutate(termination_or_transition_reason = substring(termination_or_transition_reason, 3)) %>% 
  mutate(termination_or_transition_reason = if_else(is.na(termination_or_transition_date), "PDS Active", termination_or_transition_reason)) %>%
  mutate(termination_or_transition_reason = if_else(is.na(termination_or_transition_reason), "Unknown Reason", termination_or_transition_reason)) %>%
  mutate(termination_or_transition_reason = str_trim(termination_or_transition_reason, "left"))

er<-ldp_wait_times %>% filter(termination_or_transition_reason == "Service user has terminated PDS early/refused")


# create summary
source(here("dashboard/functions/summarise_by_variable.R"))

data_wait <- summarise_pathways(ldp_wait_times)

data_wait %<>% mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb))

data_wait_2 <- summarise_pathways_2(ldp_wait_times)

data_wait_2 %<>% mutate(ijb = if_else(ijb == "All", health_board, ijb))


write_csv(data_wait, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_wait.csv")

write_csv(data_wait_2, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_wait_2.csv")


# 9 variable analysis tables ----


data_subtype <- summarise_by_variable(subtype_of_dementia)
data_stage <- summarise_by_variable(clinical_impression_of_stage_of_illness)
data_referral <- summarise_by_variable(pds_referral_source)
data_model <- summarise_by_variable(model_of_care)
data_age <- summarise_by_variable_demo(age_grp)
data_simd <- summarise_by_variable_demo(simd)
data_accom <- summarise_by_variable_demo(accommodation_type)

write_csv(data_model, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_model.csv")
write_csv(data_subtype, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_subtype.csv")
write_csv(data_stage, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_stage.csv")
write_csv(data_referral, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_referral.csv")
write_csv(data_age, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_age.csv")
write_csv(data_simd, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_simd.csv")
write_csv(data_accom, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_accom.csv")





##### END OF SCRIPT #####
