#########################################################################
# Name of file - 01_create-pub-basefile.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - March 2021
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create base file with all data required for publication.
#########################################################################


### 0 - Load environment file ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))


### 1 - Save finalised data file for revised FY ----

source(here("functions", "create-final-data.R"))

create_final_data(
  fy_final = nth(fy_in_pub, -2),
  collated_file = collated_file_path()
)


### 2 - Restructure data file ----

basefile <- read_rds(data_path(directory = "mi",
                               type = "final_data", 
                               ext = "rds")) %>% 
  
  # Select only FY to be included in pub
  filter(fy %in% fy_in_pub) %>%
  
  # Aggregate to year level (don't need month breakdown)
  group_by(across(c(health_board:simd, -month))) %>%
  summarise(referrals = sum(referrals),
            .groups = "drop") %>%
  
  # Restructure
  pivot_wider(
    names_from = ldp,
    values_from = referrals,
    values_fill = list(referrals = 0)
  ) %>%
  
  # Add summary columns
  rowwise() %>%
  mutate(
    referrals = complete + exempt + fail + ongoing,
    numerator = complete + exempt,
    denominator = referrals - ongoing
  ) %>%
  ungroup() %>%

  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                 str_sub(ijb, 11, -1))) %>% 
  # 
  # # Exclude Aberdeen city from calculating the standard but keep the number of referrals
  # # Comment this out in the future if we want to include Aberdeen City again
  # 2019/20
  mutate(complete = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, complete),
         exempt = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, exempt),
         fail = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, fail),
         ongoing = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, ongoing),
         numerator = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, numerator),
         denominator = if_else(ijb == "Aberdeen City" & fy == "2019/20", 0L, denominator)) %>% 
  # 20/21
  mutate(complete = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, complete),
         exempt = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, exempt),
         fail = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, fail),
         ongoing = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, ongoing),
         numerator = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, numerator),
         denominator = if_else(ijb == "Aberdeen City" & fy == "2020/21", 0L, denominator))

### 3 - Save file ----

write_rds(
  basefile,
  here("data", "publication", glue("{pub_date}_pub-data.rds")),
  compress = "gz"
)


### END OF SCRIPT ###