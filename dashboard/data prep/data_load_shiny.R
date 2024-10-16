################################################################################
# Name of file - data_prep_2.R
# Original Authors - Zaineb
# Updated by Abram McCormick - Sep 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - script 2 of 2 to prepare PDS data for use in R shiny. 
################################################################################
#
source(here::here("dashboard", "00_setup-environment.R"))

provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
                           "/", as.numeric(substr(last(finalised_years),6,7)) + 1)

included_years = c(finalised_years, provisional_year)


# 1 read in data from script 1 ----
#monthly_table_data <- read.csv(here("dashboard/data/monthly_table_data.csv"))
annual_table_data <- read.csv(here("dashboard/data/annual_table_data.csv"))
pds_plot_data <- read.csv(here("dashboard/data/pds_plot_data.csv"))

boards <- sort(unique(pds_plot_data$health_board))
# 2 convert data types ----

pds_plot_data %<>%
  filter(!is.na(ijb)) %>%
  mutate(ijb = factor(ijb, levels = (unique(ijb))))

annual_table_data$health_board <- as.factor(annual_table_data$health_board)
annual_table_data$ijb <- as.factor(annual_table_data$ijb)
annual_table_data$ijb <- factor(annual_table_data$ijb, levels=unique(annual_table_data$ijb))
annual_table_data$fy <- as.factor(annual_table_data$fy)
annual_table_data$ldp<- as.factor(annual_table_data$ldp)


# 3 read in additional data ----

error_summary <- read_rds(get_mi_data_path("error_data", ext = "rds", test_output = test_output)) %>% 
  mutate(ijb          = case_when(is.na(ijb) ~ "Unknown",
                                  ijb == "Scotland" ~ ijb,
                                  TRUE ~ str_sub(ijb, 11, -1))
         )
data_error <- as.data.frame(error_summary)

data_wait <- read.csv(here("dashboard/data/data_wait.csv"))
data_subtype <- read.csv(here("dashboard/data/data_subtype.csv")) 
data_model <- read.csv(here("dashboard/data/data_model.csv")) 
data_referral <- read.csv(here("dashboard/data/data_referral.csv"))
data_stage <- read.csv(here("dashboard/data/data_stage.csv"))
data_age <- read.csv(here("dashboard/data/data_age.csv"))
data_simd <- read.csv(here("dashboard/data/data_simd.csv"))
data_accom <- read.csv(here("dashboard/data/data_accom.csv"))

# 4 convert data types ----


data_wait$health_board <- as.factor(data_wait$health_board)
data_wait$fy <- as.factor(data_wait$fy)
data_wait$simd <- as.factor(data_wait$simd)
data_wait$sex <- as.factor(data_wait$sex)

# data_wait_1 <- data_wait
# data_wait_1 <- subset(data_wait_1,data_wait_1$ijb != "All")

# reshape wait times data
data_wait_reshaped <- data_wait %>%  pivot_longer(cols=c('median_diagnosis_to_referral', 'median_referral_to_contact', 'median_diagnosis_to_contact'),
                    names_to='measure',
                    values_to='median_n_days') %>% 
  mutate(measure = substring(measure, 8))

data_wait_reshaped$health_board <- as.factor(data_wait_reshaped$health_board)
data_wait_reshaped$fy <- as.factor(data_wait_reshaped$fy)
data_wait_reshaped$simd <- as.factor(data_wait_reshaped$simd)
data_wait_reshaped$sex <- as.factor(data_wait_reshaped$sex)


data_wait_reshaped <- set_colnames(data_wait_reshaped,c("health_board","IJB","fy","Gender","SIMD","Referrals","Measure","Median_days"))

data_wait_reshaped_1 <- data_wait_reshaped[data_wait_reshaped$IJB != "All",] 

data_wait$ijb <- as.factor(data_wait$ijb)
data_wait_reshaped$IJB <- as.factor(data_wait_reshaped$IJB)
data_wait_reshaped_1$IJB <- as.factor(data_wait_reshaped_1$IJB)

tabyl(data_wait_reshaped_1$IJB)

data_subtype$health_board <- as.factor(data_subtype$health_board)
data_subtype$fy <- as.factor(data_subtype$fy)
data_subtype$sex <- as.factor(data_subtype$sex)
data_subtype$type <- as.factor(data_subtype$type)

data_stage$health_board <- as.factor(data_stage$health_board)
data_stage$fy <- as.factor(data_stage$fy)
data_stage$sex <- as.factor(data_stage$sex)
data_stage$type <- as.factor(data_stage$type)

data_referral$health_board <- as.factor(data_referral$health_board)
data_referral$fy <- as.factor(data_referral$fy)
data_referral$sex <- as.factor(data_referral$sex)
data_referral$type <- as.factor(data_referral$type)

data_model$health_board <- as.factor(data_model$health_board)
data_model$fy <- as.factor(data_model$fy)
data_model$sex <- as.factor(data_model$sex)
data_model$type <- as.factor(data_model$type)

data_age$health_board <- as.factor(data_age$health_board)
data_age$fy <- as.factor(data_age$fy)
data_age$sex <- as.factor(data_age$sex)
data_age$type <- as.factor(data_age$type)

data_simd$health_board <- as.factor(data_simd$health_board)
data_simd$fy <- as.factor(data_simd$fy)
data_simd$sex <- as.factor(data_simd$sex)
data_simd$type <- as.factor(data_simd$type)

data_accom$health_board <- as.factor(data_accom$health_board)
data_accom$fy <- as.factor(data_accom$fy)
data_accom$sex <- as.factor(data_accom$sex)
data_accom$type <- as.factor(data_accom$type)



#### END OF SCRIPT ####