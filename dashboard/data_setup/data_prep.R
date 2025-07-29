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
source(here::here("code", "publication", "00_setup-pub-environment.R"))

# provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
#        "/", as.numeric(substr(last(finalised_years),6,7)) + 1)

#included_years = c(finalised_years, provisional_year)

#included_years <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")

# 2 Load PDS data ----
pds <- read_rds(get_mi_data_path("final_data", ext = "rds", test_output = test_output)) %>% 
  
  # inlcude only finalised years
 # filter(fy %in% included_years) %>% 
  
  
  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1))) %>% 
  
#re-class all aberdeen city referrals from 2019/20 and 2020/21 so they are not included in ldp calculations  
   mutate(ldp = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20","2020/21"), "Aberdeen", ldp))

# load expected diagnoses table
exp <- read_csv(get_exp_diagnoses_path())

exp %<>% select(-health_board) %>% rename("health_board" = "health_board_label") %>% mutate(ldp = "total")

# 3 Prepare data breakdowns for annual tables ----
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


# 4 calculate percentage met standard ----
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


perc_met_ijb <- full_join(num_ijb, den_ijb, by = c("health_board", "ijb", "fy")) %>%
  mutate(percent_met = round(num/den*100, 1)
         ) %>%
  select(-num, -den)


# Join percent_met onto prepared data
annual_table_data <- left_join(prepare_data, perc_met_ijb, by = c("health_board", "ijb", "fy")) %>% 
  mutate(health_board = if_else(health_board == "Scotland", "AAA Scotland", health_board)) %>% 
  arrange(ijb, health_board) %>% 
  mutate(health_board = if_else(health_board == "AAA Scotland", "Scotland", health_board)) %>% 
  mutate(ijb = if_else(ijb == "AAA", health_board, ijb))

# add expected diagnoses data

annual_table_data <- left_join(annual_table_data, exp)

annual_table_data %<>%  mutate(exp_perc = if_else(!is.na(diagnoses), round(referrals/diagnoses*100, 1), NA))


# 5 Save annual output ----
annual_table_data %>% 
write_rds("//conf/dementia/A&I/Outputs/dashboard/data/annual_table_data.rds")


# 6 read in individual data ----
ldp <- read_rds(get_mi_data_path("ldp_data", ext = "rds", test_output = test_output)) %>% 
  
  mutate(ldp = word(ldp, 1)) %>% 

# Remove codes from board and IJB
mutate(n_referrals = 1,
       health_board = str_sub(health_board, 3, -1),
       ijb          = if_else(is.na(ijb),
                              "Unknown",
                              str_sub(ijb, 11, -1)))

# remove Aberdeen 19/20 and 20/21 from standard
ldp %<>% mutate(ldp = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20","2020/21"), "Aberdeen", ldp))
  
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

# 7 variable analysis tables ----
source(here("dashboard/functions/summarise_functions_for_dashboard.R"))

data_sex <- summarise_by_variable_gender_dashboard(sex)
#data_subtype <- summarise_by_variable_dashboard(subtype_of_dementia)
#data_stage <- summarise_by_variable_dashboard(clinical_impression_of_stage_of_illness)
#data_referral <- summarise_by_variable_dashboard(pds_referral_source)
#data_model <- summarise_by_variable_dashboard(model_of_care)
data_age <- summarise_by_variable_dashboard(age_grp)
data_simd <- summarise_by_variable_dashboard(simd)
#data_accom <- summarise_by_variable_dashboard(accommodation_type)
#data_uptake <- summarise_uptake_dashboard(ldp)

write_rds(data_sex, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_sex.rds")
#write_rds(data_model, 
#          "//conf/dementia/A&I/Outputs/dashboard/data/data_model.rds")
#write_rds(data_subtype, 
#          "//conf/dementia/A&I/Outputs/dashboard/data/data_subtype.rds")
#write_rds(data_stage, 
#          "//conf/dementia/A&I/Outputs/dashboard/data/data_stage.rds")
#write_rds(data_referral, 
#          "//conf/dementia/A&I/Outputs/dashboard/data/data_referral.rds")
write_rds(data_age, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_age.rds")
write_rds(data_simd, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_simd.rds")
#write_rds(data_accom, 
#          "//conf/dementia/A&I/Outputs/dashboard/data/data_accom.rds")
#write_rds(data_uptake, 
#        "//conf/dementia/A&I/Outputs/dashboard/data/data_uptake.rds")

  

# 8 waiting times ----

data_wait <- read_rds(get_mi_data_path("wait_data", ext = "rds", test_output = test_output)) %>% 
   mutate(ijb = if_else(ijb == "All", health_board, ijb))

write_rds(data_wait, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_wait.rds")

# 9 pop data
data_pop <- read_rds("//conf/dementia/A&I/Outputs/management-report/lookups/pop_data.rds")

write_rds(data_pop, 
          "//conf/dementia/A&I/Outputs/dashboard/data/data_pop.rds")

# 10 prepare data for download page ----
download_data <- pds %>% 
  
  #remove code from gender and combine unknowns
  mutate(sex = if_else(sex == "Unknown", sex, substring(sex, 4))) %>% 
  mutate(sex = if_else(is.na(sex) | sex == "Not Known", "Unknown", sex)) %>% 
  
  # Aggregate to year level (don't need month breakdown)
  group_by(across(c(health_board:sex, -month))) %>%
  summarise(referrals = sum(referrals),
            .groups = "drop") %>% 
  ungroup()
  
### Scotland----
scotland_ldp <- bind_rows(
  # all referrals
  download_data %>% 
  group_by(health_board = "Scotland", fy, simd = "All", sex = "All", age_grp = "All", ldp) %>% summarise(referrals = sum(referrals)) %>%
  ungroup() %>% 
  select(geog = health_board, fy, sex, age_grp, simd, ldp, referrals) %>% 
  
# Restructure
pivot_wider(
  names_from = ldp,
  values_from = referrals,
  values_fill = list(referrals = 0)
) %>%
  
  # Add summaries columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>% 
  
  mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
  select(geog, fy, sex, age_grp, simd, referrals, complete, exempt, ongoing, fail, percent_met),

#simd breakdown
download_data %>% 
group_by(health_board = "Scotland", fy, simd, sex = "All", age_grp = "All", ldp) %>% summarise(referrals = sum(referrals)) %>%
ungroup() %>%

select(geog = health_board, fy, sex, age_grp, simd, ldp, referrals) %>% 
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  
  # Add summaries columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>% 
  
  mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
  select(geog, fy, sex, age_grp, simd, referrals, complete, exempt, ongoing, fail, percent_met),

#gender breakdown
download_data %>% 
  group_by(health_board = "Scotland", fy, simd = "All", sex, age_grp = "All", ldp) %>% summarise(referrals = sum(referrals)) %>%
  ungroup() %>%
  
  select(geog = health_board, fy, sex, age_grp, simd, ldp, referrals) %>% 
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  
  # Add summaries columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>% 
  
  mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
  select(geog, fy, sex, age_grp, simd, referrals, complete, exempt, ongoing, fail, percent_met),

#age breakdown
download_data %>% 
  group_by(health_board = "Scotland", fy, simd = "All", sex = "All", age_grp, ldp) %>% summarise(referrals = sum(referrals)) %>%
  ungroup() %>%
  
  select(geog = health_board, fy, sex, age_grp, simd, ldp, referrals) %>% 
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  
  # Add summaries columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>% 
  
  mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
  select(geog, fy, sex, age_grp, simd, referrals, complete, exempt, ongoing, fail, percent_met)
)


scotland_ldp_exp <- left_join(scotland_ldp, 
                                exp %>% filter(health_board == "Scotland") %>% 
                                      mutate(sex = "All", age_grp = "All", simd = "All") %>% 
                                      select(geog = health_board, fy, sex, age_grp, simd, diagnoses)) %>% 
  mutate(exp_perc = if_else(!is.na(diagnoses), round(referrals/diagnoses*100, 1), NA))

download_data_scotland <- left_join(scotland_ldp_exp, 
                                    test<-data_wait %>% filter(health_board == "Scotland", sex == "All", simd == "All") %>% 
                                      mutate(age_grp = "All") %>% 
                                      select(geog = health_board, fy, sex, age_grp, simd, perc_contacted, median_diagnosis_to_contact)) %>% 
  relocate(c(diagnoses, exp_perc), .after = referrals) %>% 
  
  rename(geography = geog,
         financial_year = fy,
         gender = sex,
         age_group = age_grp,
         deprivation_quintile = simd,
         `number of people referred to PDS` = referrals,
         `LDP standard part 1 - estimated number of people newly diagnosed with dementia` = diagnoses,
         `LDP standard part 1 - percentage of estimated number of people diagnosed with dementia referred to pds` = exp_perc,
         `LDP standard part 2 - number of people where standard was met` = complete,
         `LDP standard part 2 - number of people exempt from standard` = exempt,
         `LDP standard part 2 - number of people where PDS is ongoing` = ongoing,
         `LDP standard part 2 - number of people where standard was not met` = fail,
         `LDP standard part 2 - percentage of LDP standard achieved` = percent_met,
         `percentage of referrals contacted by PDS practitioner` = perc_contacted,
         `average (median) days from diagnoses to first contact` = median_diagnosis_to_contact)

download_data_scotland %<>% pivot_longer(cols = c(`number of people referred to PDS`:`average (median) days from diagnoses to first contact`), names_to = "measure")

write_rds(download_data_scotland, 
          "//conf/dementia/A&I/Outputs/dashboard/data/download_data_scotland.rds")

### healthboards----
hb_ldp <- 
  # all referrals
  download_data %>% 
    group_by(health_board, fy, simd = "All", sex = "All", age_grp = "All", ldp) %>% summarise(referrals = sum(referrals)) %>%
    ungroup() %>% 
    select(health_board, fy, ldp, referrals) %>% 
    
    # Restructure
    pivot_wider(
      names_from = ldp,
      values_from = referrals,
      values_fill = list(referrals = 0)
    ) %>%
    
    # Add summaries columns
    rowwise() %>%
    mutate(
      referrals = complete + exempt + fail + ongoing + Aberdeen,
      numerator = complete + exempt,
      denominator = complete + exempt + fail
    ) %>%
    ungroup() %>% 
    
    mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
    select(health_board, fy, referrals, complete, exempt, ongoing, fail, percent_met)

hb_ldp_exp <- left_join(hb_ldp, 
                            exp %>% select(health_board, fy, diagnoses)) %>% 
  mutate(exp_perc = if_else(!is.na(diagnoses), round(referrals/diagnoses*100, 1), NA))

download_data_hb <- left_join(hb_ldp_exp, 
                                   data_wait %>% filter(grepl("NHS",ijb),sex == "All", simd == "All") %>% 
                                      select(health_board, fy, perc_contacted, median_diagnosis_to_contact)) %>% 
  mutate(median_diagnosis_to_contact = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), NA, median_diagnosis_to_contact)) %>% 
  mutate(perc_contacted = if_else(health_board == "NHS Grampian" & fy %in% c("2019/20", "2020/21"), NA, perc_contacted)) %>% 
  relocate(c(diagnoses, exp_perc), .after = referrals) %>% 
  
  rename(geography = health_board,
         financial_year = fy,
         `number of people referred to PDS` = referrals,
         `LDP standard part 1 - estimated number of people newly diagnosed with dementia` = diagnoses,
         `LDP standard part 1 - percentage of estimated number of people diagnosed with dementia referred to pds` = exp_perc,
         `LDP standard part 2 - number of people where standard was met` = complete,
         `LDP standard part 2 - number of people exempt from standard` = exempt,
         `LDP standard part 2 - number of people where PDS is ongoing` = ongoing,
         `LDP standard part 2 - number of people where standard was not met` = fail,
         `LDP standard part 2 - percentage of LDP standard achieved` = percent_met,
         `percentage of referrals contacted by PDS practitioner` = perc_contacted,
         `average (median) days from diagnoses to first contact` = median_diagnosis_to_contact)

download_data_hb %<>% pivot_longer(cols = c(`number of people referred to PDS`:`average (median) days from diagnoses to first contact`), names_to = "measure")

write_rds(download_data_hb, 
          "//conf/dementia/A&I/Outputs/dashboard/data/download_data_hb.rds")

### integration authority areas----
ijb_ldp <- 
  # all referrals
  download_data %>% 
  group_by(ijb, fy, simd = "All", sex = "All", age_grp = "All", ldp) %>% summarise(referrals = sum(referrals)) %>%
  ungroup() %>% 
  select(ijb, fy, ldp, referrals) %>% 
  
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  
  # Add summaries columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing + Aberdeen,
    numerator = complete + exempt,
    denominator = complete + exempt + fail
  ) %>%
  ungroup() %>% 
  
  mutate(percent_met = round_half_up(100*numerator/denominator, 1)) %>% 
  select(ijb, fy, referrals, complete, exempt, ongoing, fail, percent_met)

download_data_ijb <- left_join(ijb_ldp, 
                              data_wait %>% filter(sex == "All", simd == "All") %>% 
                                select(ijb, fy, perc_contacted, median_diagnosis_to_contact)) %>% 
  mutate(median_diagnosis_to_contact = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), NA, median_diagnosis_to_contact)) %>% 
  mutate(perc_contacted = if_else(ijb == "Aberdeen City" & fy %in% c("2019/20", "2020/21"), NA, perc_contacted))  %>% 
  rename(geography = ijb,
         financial_year = fy,
         `number of people referred to PDS` = referrals,
         `LDP standard part 2 - number of people where standard was met` = complete,
         `LDP standard part 2 - number of people exempt from standard` = exempt,
         `LDP standard part 2 - number of people where PDS is ongoing` = ongoing,
         `LDP standard part 2 - number of people where standard was not met` = fail,
         `LDP standard part 2 - percentage of LDP standard achieved` = percent_met,
         `percentage of referrals contacted by PDS practitioner` = perc_contacted,
         `average (median) days from diagnoses to first contact` = median_diagnosis_to_contact)

download_data_ijb %<>% pivot_longer(cols = c(`number of people referred to PDS`:`average (median) days from diagnoses to first contact`), names_to = "measure")

write_rds(download_data_ijb, 
          "//conf/dementia/A&I/Outputs/dashboard/data/download_data_ijb.rds")

##### END OF SCRIPT #####

