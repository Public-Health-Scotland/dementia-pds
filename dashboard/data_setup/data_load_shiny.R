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

# un-comment below when testing this script
# source(here::here("code", "publication", "00_setup-pub-environment.R"))


# 1 read in data from data_prep.R ----

annual_table_data <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/annual_table_data.rds")
data_wait <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/data_wait.rds")
data_age <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/data_age.rds")
data_simd <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/data_simd.rds")
data_sex <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/data_sex.rds")
data_rates <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/data_rates.rds")
download_data_scotland <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/download_data_scotland.rds")
download_data_hb <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/download_data_hb.rds")
download_data_ijb <- read_rds("//conf/dementia/A&I/Outputs/dashboard/data/download_data_ijb.rds")

# 2 add superscript to provisional and revised years and convert to factors----
provisional_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 1,
                           "/", as.numeric(substr(last(finalised_years),6,7)) + 1)


revised_year <- paste0(as.numeric(substr(last(finalised_years),1,4)),
                       "/", as.numeric(substr(last(finalised_years),6,7)))

revised_year_extra <- paste0(as.numeric(substr(last(finalised_years),1,4)) - 1,
                       "/", as.numeric(substr(last(finalised_years),6,7)))


extra_referrals_year <- paste0(as.numeric(substr(last(finalised_years),1,4)) + 2,
                               "/", as.numeric(substr(last(finalised_years),6,7)) + 2)


#download_data ----
#add superscripts
download_data_scotland<-download_data_scotland %>% 
  mutate(financial_year = case_when(financial_year == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                    financial_year == revised_year ~paste0(revised_year,"ᴿ"),
                                    financial_year == extra_referrals_year ~paste0(extra_referrals_year ,"ᴾ"),
                                    TRUE ~financial_year))
  
download_data_hb<-download_data_hb %>% 
  mutate(financial_year = case_when(financial_year == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                    financial_year == revised_year ~paste0(revised_year,"ᴿ"),
                                    financial_year == extra_referrals_year ~paste0(extra_referrals_year ,"ᴾ"),
                                    TRUE ~financial_year))
download_data_ijb<-download_data_ijb %>% 
  mutate(financial_year = case_when(financial_year == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                    financial_year == revised_year ~paste0(revised_year,"ᴿ"),
                                    financial_year == extra_referrals_year ~paste0(extra_referrals_year ,"ᴾ"),
                                    TRUE ~financial_year))

# yearly referrals and ldp data
#add superscripts
annual_table_data <- annual_table_data %>%  
  
  mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                       fy == revised_year ~paste0(revised_year,"ᴿ"),
                       fy == extra_referrals_year ~paste0(extra_referrals_year ,"ᴾ"),
                       TRUE ~fy))


annual_table_data$ijb <- factor(annual_table_data$ijb, levels=unique(annual_table_data$ijb))
annual_table_data$health_board <- factor(annual_table_data$health_board, levels=unique(annual_table_data$health_board))
annual_table_data$ldp<- as.factor(annual_table_data$ldp)
annual_table_data$fy <- as.factor(annual_table_data$fy)


#data_wait----
#filter simd and sex to All and add superscripts
data_wait <- data_wait %>% 
  mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                        #UNCOMMENT line below in 2026----
                        # fy == revised_year ~paste0(revised_year,"ᴿ"),
                        TRUE ~fy))

data_wait$health_board <- factor(data_wait$health_board, levels=unique(data_wait$health_board))
data_wait$ijb <- factor(data_wait$ijb, levels=unique(annual_table_data$ijb))
data_wait$fy <- as.factor(data_wait$fy)

#data_age----
#filter sex to All and add superscripts
data_age <- data_age %>% mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                               fy == revised_year ~paste0(revised_year,"ᴿ"),
                                               TRUE ~fy)) 

data_age$health_board <- factor(data_age$health_board, levels=unique(data_age$health_board))
data_age$ijb <- factor(data_age$ijb, levels=unique(annual_table_data$ijb))
data_age$type <- as.factor(data_age$type)
data_age$fy <- as.factor(data_age$fy)

#data_sex----
#filter simd to All and add superscripts
data_sex <- data_sex %>% mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                               #UNCOMMENT line below in 2026----
                                               # fy == revised_year ~paste0(revised_year,"ᴿ"),
                                               TRUE ~fy))  

data_sex$health_board <- factor(data_sex$health_board, levels=unique(data_sex$health_board))
data_sex$ijb <- factor(data_sex$ijb, levels=unique(annual_table_data$ijb))
data_sex$type <- factor(data_sex$type, levels = c("Male", "Female", "Not Specified", "Unknown"))
data_sex$fy <- as.factor(data_sex$fy)


#data_simd----
#filter sex to All and add superscripts
data_simd <- data_simd %>% mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                                 fy == revised_year ~paste0(revised_year,"ᴿ"),
                                                 TRUE ~fy))

data_simd$health_board <- factor(data_simd$health_board, levels=unique(data_simd$health_board))
data_simd$ijb <- factor(data_simd$ijb, levels=unique(annual_table_data$ijb))
data_simd$type <- as.factor(data_simd$type)
data_simd$fy <- as.factor(data_simd$fy)

#data_rates----
# add superscripts

data_rates <- data_rates %>% mutate(fy = case_when(fy == provisional_year ~paste0(provisional_year ,"ᴾ"),
                                     #UNCOMMENT line below in 2026----
                                     #fy == revised_year ~paste0(revised_year,"ᴿ"),
                                     fy == extra_referrals_year ~paste0(extra_referrals_year ,"ᴾ"),
                                     TRUE ~fy))

data_rates$health_board <- factor(data_rates$health_board, levels=unique(data_rates$health_board))
data_rates$ijb <- factor(data_rates$ijb, levels=unique(annual_table_data$ijb))
data_rates$fy <- as.factor(data_rates$fy)


#### END OF SCRIPT ####