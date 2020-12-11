#########################################################################
# Name of file - 02_create-excel-tables.R
# Data release - Annual Dementia PDS Publication
# Original Authors - Alice Byers
# Original Date - February 2020
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Restructure data and save to tab in excel template
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Load data ----

pds <-
  read_rds(here("data", 
                glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                glue("{fy}-{qt}_final-data.rds")))

expected <-
  read_csv(here("reference-files",
                "expected-diagnoses.csv")) %>%
  select(-health_board) %>%
  mutate(lookup = paste0(fy, health_board_label), .before = everything())


### 3 - Restructure data ----

pds %<>%
  filter(fy %in% fy_in_pub) %>%
  mutate(health_board = substring(health_board, 3),
         ijb = substring(ijb, 11))


excel_data <-
  
  bind_rows(
    
    # Health Board
    pds %>% 
      group_by(fy, category = "hb", category_split = health_board, ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>%
      pivot_wider(names_from = ldp, 
                  values_from = referrals,
                  values_fill = list(referrals = 0)) %>%
      arrange(fy, category, category_split),
    
    # Scotland Rows
    pds %>% 
      group_by(fy, category = "hb", category_split = "Scotland", ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>% 
      pivot_wider(names_from = ldp, 
                  values_from = referrals, 
                  values_fill = list(referrals = 0)),
    
    # IJB
    pds %>% 
      group_by(fy, category = "ijb", category_split = ijb, ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>% 
      pivot_wider(names_from = ldp, 
                  values_from = referrals, 
                  values_fill = list(referrals = 0)) %>%
      arrange(fy, category, category_split),
    
    pds %>% 
      group_by(fy, category = "ijb", category_split = "Scotland", ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>%
      pivot_wider(names_from = ldp, 
                  values_from = referrals,
                  values_fill = list(referrals = 0)),
    
    # Age Group
    pds %>% 
      group_by(fy, category = "age", category_split = age_grp, ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>%
      pivot_wider(names_from = ldp, 
                  values_from = referrals,
                  values_fill = list(referrals = 0)) %>%
      arrange(fy, category, category_split),
    
    # Deprivation
    pds %>% 
      group_by(fy, category = "simd", category_split = simd, ldp) %>% 
      summarise(referrals = sum(referrals), .groups = "drop") %>%
      pivot_wider(names_from = ldp, 
                  values_from = referrals,
                  values_fill = list(referrals = 0)) %>%
      arrange(fy, category, category_split)
    
  ) %>%
  
  filter(!is.na(category_split)) %>%
  mutate(referrals = reduce(select(., complete:ongoing), `+`),
         rate = (complete + exempt) / (referrals - ongoing)) %>%
  
  # Add lookup column
  mutate(lookup = paste0(fy, category, category_split), .before = everything())


### 4 - Save data to excel template ----

wb <- loadWorkbook(here("reference-files",
                        "excel-template.xlsx"))

writeData(wb,
          "data",
          excel_data,
          startCol = 1)

writeData(wb,
          "expected",
          expected,
          startCol = 1)

sheetVisibility(wb)[8:10] <- "hidden"

saveWorkbook(wb,
             here("publication", "output", pub_date, 
                  glue("{pub_date}_excel-tables.xlsx")),
             overwrite = TRUE)


### END OF SCRIPT ###