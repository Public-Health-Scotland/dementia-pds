#########################################################################
# Name of file - 05_create-discovery-data.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - March 2020
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Restructure data for Discovery LDP data submission
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))


### 2 - Load data ----

pds <-
  read_rds(here("data", 
                glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                glue("{fy}-{qt}_final-data.rds")))


### 3 - Restructure data ----

ldp <-
  
  pds %>%
  filter(fy %in% fy_in_pub) %>%
  
  group_by(fy, health_board) %>%
  summarise(
    num = sum(case_when(
      ldp %in% c("complete", "exempt") ~ referrals,
      TRUE ~ 0
    )),
    denom = sum(case_when(
      ldp != "ongoing" ~ referrals,
      TRUE ~ 0
    )),
    .groups = "drop"
  ) %>%
  
  group_by(fy) %>%
  group_modify(~ adorn_totals(., name = "Scotland")) %>%
  
  mutate(fy = dmy(paste0("310320", substr(fy, 6, 7))),
         health_board = str_remove(health_board, "^[A-Za-z]\\s"),
         Indicator = (num / denom) * 100,
         Indicator_ID = 21,
         Frequency = "Y",
         Indicator_Name = "Dementia Post Diagnostic Support",
         Standard_Level = "") %>%
  
  select(Indicator_ID, Frequency, Period_End_Date = fy,
         Indicator_Name, Organisation = health_board, Numerator_Value = num,
         Denominator_Value = denom, Indicator, Standard_Level) %>%
  
  arrange(Period_End_Date, Organisation)


### 4 - Save data ----

write_csv(
  ldp,
  here("publication", "output", pub_date, glue("{pub_date}_ldp-data.csv"))
)


### END OF SCRIPT ###