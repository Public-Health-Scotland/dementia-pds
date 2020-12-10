#########################################################################
# Name of file - 03_create-discovery-data.R
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

source(here::here("code", "00_setup-environment.R"))


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
  filter(ldp != "ongoing") %>%
  mutate(denom = sum(referrals)) %>%
  filter(ldp %in% c("complete", "exempt")) %>%
  summarise(num = sum(referrals),
            denom = max(denom),
            .group = "drop") %>%
  
  bind_rows(
    pds %>%
      filter(fy %in% fy_in_pub) %>%
      
      group_by(fy, health_board = "Scotland") %>%
      filter(ldp != "ongoing") %>%
      mutate(denom = sum(referrals)) %>%
      filter(ldp %in% c("complete", "exempt")) %>%
      summarise(num = sum(referrals),
                denom = max(denom),
                .groups = "drop")
  ) %>%
  
  mutate(fy = dmy(paste0("310320", substr(fy, 6, 7))),
         health_board = str_remove(health_board, "^[A-Za-z]\\s"),
         Indicator = (num / denom) * 100,
         Indicator_ID = 21,
         Frequency = "Y",
         Indicator_Name = "Dementia Post Diagnostic Support",
         Standard_Level = "") %>%
  
  rename(Period_End_Date = fy,
         Organisation = health_board,
         Numerator_Value = num,
         Denominator_Value = denom) %>%
  
  select(Indicator_ID, Frequency, Period_End_Date,
         Indicator_Name, Organisation, Numerator_Value,
         Denominator_Value, Indicator, Standard_Level) %>%
  
  arrange(Period_End_Date, Organisation)


### 4 - Save data ----

write_csv(
  ldp,
  here("publication", "output", glue("{pub_date}_ldp-data.csv"))
)


### END OF SCRIPT ###