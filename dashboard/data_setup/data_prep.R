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

rm(exp)
rm(board)
rm(prepare_data)
rm(pds)
rm(den_ijb)
rm(num_ijb)
rm(pds_rate_ijb)



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
               "carers_support",                         
               "termination_or_transition_reason")

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

# create summary
data_wait <- bind_rows(
  
ldp_wait_times %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd = "All") %>% 
   summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
             median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
             median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb = "All", fy, sex = "All", simd = "All") %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb, fy, sex = "All", simd = "All") %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb, fy, sex, simd = "All") %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb, fy, sex, simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd = "All") %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb = "All", fy, sex, simd = "All") %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board = "Scotland", ijb = "All", fy, sex, simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb = "All", fy, sex, simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board = "Scotland", ijb = "All", fy, sex = "All", simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb = "All", fy, sex = "All", simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop"),

ldp_wait_times %>% group_by(health_board, ijb, fy, sex = "All", simd) %>% 
  summarise(median_diagnosis_to_referral = median(diagnosis_to_referral_days, na.rm = T),
            median_referral_to_allocation = median(referral_to_allocation_days, na.rm = T),
            median_allocation_to_contact = median(allocation_to_contact_days, na.rm = T),
            median_referral_to_contact = median(referral_to_contact_days, na.rm = T),
            median_diagnosis_to_contact = median(diagnosis_to_contact_days, na.rm = T),
            median_contact_to_termination = median(contact_to_termination_days, na.rm = T),
            total_referrals = sum(n_referrals), .groups = "drop")
  )

data_wait %<>% mutate(ijb = if_else(health_board == "Scotland", "Scotland", ijb))

write_csv(data_wait, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_wait.csv")

rm(ldp_wait_times)

# 9 variable analysis tables ----

source(here("dashboard/functions/summarise_by_variable.R"))

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

rm(ldp)




##### END OF SCRIPT #####





# ldp %>% group_by(health_board, accommodation_type) %>%
#   summarise(total_referrals = sum(n_referrals),
#             exempt_referrals = sum(ldp == "exempt"))


# vb_2_data<- annual_table_data %>% filter(health_board == "Scotland", ijb == "Scotland", fy == provisional_year,
#                                        ldp == "complete" | ldp == "exempt" | ldp == "total") %>% select(-diagnoses) %>% 
#                           pivot_wider(values_from = referrals, names_from = ldp)
# 
# 
# 
# vb_text <- paste0(round(100*(vb_2_data$complete + vb_2_data$exempt)/vb_2_data$total, 1), "%")
# 
# 
# annual_table_data %>%
#   filter(grepl("NHS", ijb) | ijb == "Scotland") %>% 
#   filter(fy == provisional_year) %>%
#   group_by(health_board)%>%
#   select(health_board,ldp,referrals,rate)%>%
#   pivot_wider(names_from=ldp,values_from=referrals) %>% relocate(total, .after = health_board) %>%
#   mutate(health_board = if_else(health_board == "Scotland","AAA Scotland", health_board)) %>% 
#   arrange(health_board) %>% 
#   mutate(health_board = if_else(health_board == "AAA Scotland","Scotland", health_board)) %>% 
#   formatCurrency(., currency = "", interval = 3, mark = ",") %>% 
#   set_colnames(c("Health Board","Total Referrals", "% Met Standard/Exempt","Standard Met","Exempt","Standard Not Met","PDS Ongoing")) 

# data_wait %>% filter(fy == provisional_year, sex == "All", ijb == "All", simd == "All") %>% 
#   select(health_board, total_referrals, median_diagnosis_to_referral, median_referral_to_contact, median_diagnosis_to_contact)


# test<-annual_table_data %>% 
#    filter(health_board == "NHS Fife" | health_board == "Scotland")
# 
# plot_trend(test)
# 
# test %<>%
#   
#   select(health_board, fy, rate) %>%
#   distinct(health_board, fy, .keep_all = T) %>% 
#   mutate(rate = as.numeric(substr(rate,1,5)))
# 
# 
# plot <- test %>%
#   
#   ggplot(aes(x = fy,
#              y = rate,
#              group = health_board,
#              text = paste0(health_board, "<br>",
#                            fy, "<br>",
#                            rate, "%")))
# 
# plot
# data_simd %>% filter(health_board == "Scotland", fy == provisional_year, sex == "All") %>% 
# 
# proportion_bar_chart()

# test<- data_simd%>% filter(health_board == "Scotland", fy == provisional_year, sex == "All")
# 
# 
# test %>% 
#   select(type, total_referrals, complete, exempt, ongoing, not_met, percent_met) %>% 
#   mutate(percent_met = paste0(percent_met,"%")) %>% 
#   set_colnames(c("","Number of People Referred to PDS", "Standard Met","Exempt from Standard","PDS Ongoing", "Standard Not Met", "Percentage of LDP standard achieved"))