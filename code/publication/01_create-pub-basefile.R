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


### 1 - Load environment file ----

source(here::here("code", "publication", "00_setup-pub-environment.R"))


### 2 - Restructure data file ----

basefile <- 
  
  read_rds(
    here("data", 
         glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
         glue("{fy}-{qt}_final-data.rds"))
  ) %>%
  
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
    numerator = complete + exempt
  ) %>%
  ungroup() %>%

  # Remove codes from board and IJB
  mutate(health_board = str_sub(health_board, 3, -1),
         ijb          = if_else(is.na(ijb),
                                "Unknown",
                                str_sub(ijb, 11, -1)))


### 3 - Save file ----

write_rds(
  basefile,
  here("data", "publication", glue("{pub_date}_pub-data.rds")),
  compress = "gz"
)


### END OF SCRIPT ###