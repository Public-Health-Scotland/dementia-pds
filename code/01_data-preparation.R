#########################################################################
# Name of file - 01_data-preparation.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Read in data file from data management and prepare for
# adding measures and producing outputs.
#########################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Read and clean collated file ----

pds <- 
  
  read_csv(glue("{stats}/dementia/03-Outputs/National/",
                "{fy}-Q{qt}_national.csv"),
           col_types = cols(.default = "c")) %>%
  
  clean_names() %>%
  
  # Remove empty 'x' variables (TEMP - move this to DM script)
  select(-starts_with("x")) %>%
  
  # Convert dates from character to date format
  mutate(across(contains("date"), ymd)) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = chi_pad(chi_number)) %>%
  
  # Replace word 'and' with ampersand
  mutate(health_board = str_replace(health_board, " and ", " & ")) %>%
  
  # Remove records with missing diag date or outwith reporting period
  filter(between(dementia_diagnosis_confirmed_date, start_date, end_date))


### 3 - Add on final data no longer submitted ----

pds <-
  
  # Read in and bind all previous finalised years data
  list.files(here("data", "final"), full.names = TRUE) %>%
  map(read_rds) %>%
  reduce(bind_rows) %>%
  
  # Add to latest submission data
  bind_rows(pds)


### 3 - Save out error summary

err <- pds %>%
  mutate(health_board = if_else(is.na(health_board),
                                "Missing",
                                substring(health_board, 3))) %>%
  mutate(fy = fin_year(dementia_diagnosis_confirmed_date)) %>%
  group_by(fy, health_board, ijb) %>%
  summarise(total_errors     = sum(as.integer(error_flag)),
            records          = n(),
            .groups = "drop") %>%
  group_by(fy) %>%
  group_modify(
    ~ bind_rows(.x, summarise(.x,
                              health_board = "Scotland",
                              ijb = "Scotland",
                              across(c(total_errors, records), sum)))
  ) %>%
  ungroup() %>%
  arrange(fy, health_board, ijb) %T>%
  write_rds(here("data", 
                 glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                 glue("{fy}-{qt}_error-summary.rds")))


### 4 - Recode Lanarkshire IJB records ----

pds %<>%
  mutate(health_board = 
            case_when(
               str_detect(ijb, "S37000035|S37000028") ~ "L NHS Lanarkshire",
                          TRUE                        ~ health_board
            ))


### 5 - Remove duplicate records ----

pds %<>%

  group_by(chi_number) %>%
  
  # Add duplicate flag
  mutate(dupe = if_else(!is.na(chi_number) & n() > 1, 1 , 0)) %>%
  
  # Add flag for GGC/H duplicates
  mutate(ggc_h_dupe = 
           if_else(!is.na(chi_number) & n() > 1 &
                     all(c("G NHS Greater Glasgow & Clyde",
                           "H NHS Highland") %in% 
                           health_board),
                   1,
                   0)) %>% 
  
  # Add priority flag for duplicates to keep
  # Earliest diagnosis date, Term reason 04, Earliest contact date
  mutate(dupe_keep = 

           case_when(
             
             ggc_h_dupe == 1 & str_detect(health_board, "^G") ~ 1,
             
             ggc_h_dupe == 1 & !str_detect(health_board, "^G") ~ 0,
             
             dupe == 1 &
               # Select earliest diag date where different
               n_distinct(dementia_diagnosis_confirmed_date) > 1 &
               dementia_diagnosis_confirmed_date == min(dementia_diagnosis_confirmed_date, na.rm = TRUE) ~ 1,
             
             dupe == 1 &
               # Select earliest diag date where different
               n_distinct(dementia_diagnosis_confirmed_date) > 1 &
               (!(dementia_diagnosis_confirmed_date == min(dementia_diagnosis_confirmed_date, na.rm = TRUE)) |
               is.na(dementia_diagnosis_confirmed_date)) ~ 0,
             
             dupe == 1 &
               # Select termination reason 04 Moved to different Health Board
               n_distinct(termination_or_transition_reason) > 1 &
               str_detect(termination_or_transition_reason, "^04") ~ 1,
             
             dupe == 1 &
             # Select termination reason 04 Moved to different Health Board
             n_distinct(termination_or_transition_reason) > 1 &
               any(str_detect(termination_or_transition_reason, "^04")) ~ 0,
             
             dupe == 1 &
               # Select earliest contact date
               n_distinct(date_of_initial_first_contact) > 1 &
               date_of_initial_first_contact == min(date_of_initial_first_contact, na.rm = TRUE) ~ 1,
             
             dupe == 1 &
               # Select earliest contact date
               n_distinct(date_of_initial_first_contact) > 1 &
               (!(date_of_initial_first_contact == min(date_of_initial_first_contact, na.rm = TRUE)) |
               is.na(date_of_initial_first_contact)) ~ 0,
             
             dupe != 1 ~ 1
             
           )) %>%
  
  ungroup()


# Save duplicate records
dupes <- 
  pds %>% 
  filter(dupe == 1) %T>%
  write_csv(here("data", 
                 glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                 glue("{fy}-{qt}_dupes.csv")))


# Remove duplicate records
pds %<>%
  
  filter(dupe_keep == 1) %>%
  
  # Recode GGC duplicate as Argyll & Bute activity
  mutate(health_board = if_else(ggc_h_dupe == 1, "H NHS Highland", health_board),
         ijb = if_else(ggc_h_dupe == 1, "S37000004 Argyll and Bute", ijb)) %>%
  
  select(-contains("dupe"))


### 6 - Save data ---

write_rds(pds, here("data", 
                    glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                    glue("{fy}-{qt}_clean-data.rds")))


### END OF SCRIPT ###