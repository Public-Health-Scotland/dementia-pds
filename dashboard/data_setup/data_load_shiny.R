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




# 1 read in data from script 1 ----
#monthly_table_data <- read.csv(here("dashboard/data/monthly_table_data.csv"))
annual_table_data <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/annual_table_data.csv")
pds_plot_data <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/pds_plot_data.csv")
data_wait <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_wait.csv")
data_wait_2 <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_wait_2.csv")
data_subtype <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_subtype.csv")
data_model <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_model.csv")
data_referral <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_referral.csv")
data_stage <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_stage.csv")
data_age <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_age.csv")
data_simd <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_simd.csv")
data_accom <- read.csv("//conf/dementia/A&I/Outputs/dashboard/data/data_accom.csv")


# 2 read in error data ----


err <- read_rds(get_mi_data_path("error_data", ext = "rds", test_output = test_output)) %>% 
  
  mutate(ijb = if_else(is.na(ijb),
                       "Unknown",
                       str_sub(ijb, 11, -1)))

# 3 convert data types ----


pds_plot_data %<>%
  filter(!is.na(ijb)) %>%
  mutate(ijb = factor(ijb, levels = (unique(ijb))))

annual_table_data$health_board <- as.factor(annual_table_data$health_board)
annual_table_data$ijb <- as.factor(annual_table_data$ijb)
annual_table_data$ijb <- factor(annual_table_data$ijb, levels=unique(annual_table_data$ijb))
annual_table_data$fy <- as.factor(annual_table_data$fy)
annual_table_data$ldp<- as.factor(annual_table_data$ldp)

data_wait$health_board <- as.factor(data_wait$health_board)
data_wait$fy <- as.factor(data_wait$fy)
data_wait$simd <- as.factor(data_wait$simd)
data_wait$sex <- as.factor(data_wait$sex)
data_wait$ijb <- as.factor(data_wait$ijb)


data_wait_2$health_board <- as.factor(data_wait_2$health_board)
data_wait_2$fy <- as.factor(data_wait_2$fy)
data_wait_2$termination_or_transition_reason <- factor(data_wait_2$termination_or_transition_reason,
                                                       levels = unique(data_wait_2$termination_or_transition_reason)[c(1,2,12,10,8,15,11,6,13,3,9,16,5,7,14,4)])
data_wait_2$sex <- as.factor(data_wait_2$sex)
data_wait_2$ijb <- as.factor(data_wait_2$ijb)
data_wait_2 %<>% arrange(termination_or_transition_reason) 

data_subtype$health_board <- as.factor(data_subtype$health_board)
data_subtype$fy <- as.factor(data_subtype$fy)
data_subtype$sex <- as.factor(data_subtype$sex)
data_subtype$type <- as.factor(data_subtype$type)

data_stage$health_board <- as.factor(data_stage$health_board)
data_stage$fy <- as.factor(data_stage$fy)
data_stage$sex <- as.factor(data_stage$sex)
data_stage$type <- factor(data_stage$type, levels = c("Early (Mild) Stage", "Mid (Moderate) Stage", "Late (Severe) Stage", "Yet to be determined", "Not Known"))
data_stage %<>% arrange(type) 


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