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

pds <- read_rds(get_pub_data_path(test_output = test_output))


### 3 - Restructure data ----

discovery <-
  
  pds %>%
  
  group_by(fy, health_board) %>%
  summarise(across(c(numerator, denominator), sum), .groups = "drop") %>%
  
  group_by(fy) %>%
  group_modify(~ adorn_totals(., name = "Scotland")) %>%
  ungroup() %>%
  
  mutate(fy = dmy(paste0("310320", substr(fy, 6, 7))),
         Indicator = (numerator / denominator) * 100,
         Indicator_ID = 21,
         Frequency = "Y",
         Indicator_Name = "Dementia Post Diagnostic Support",
         Standard_Level = "") %>%
  
  select(Indicator_ID, Frequency, Period_End_Date = fy,
         Indicator_Name, Organisation = health_board, 
         Numerator_Value = numerator,
         Denominator_Value = denominator, Indicator, Standard_Level) %>%
  
  arrange(Period_End_Date, Organisation)


### 4 - Save data ----

write_file(
  discovery,
  get_pub_output_path(output_name = "discovery_data", test_output = test_output))
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.


### END OF SCRIPT ###