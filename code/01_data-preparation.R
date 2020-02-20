#########################################################################
# Name of file - 01_data-preparation.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Orginal Date - July 2019
#
# Written/run on - RStudio Desktop
# Version of R - 3.5.1
#
# Description - Read in data file from data management and prepare for
# adding measures and producing outputs.
#
# Approximate run time - xx minutes
#########################################################################


### 1 - Load environment file and functions ----

source(here::here("code", "00_setup-environment.R"))
source(here::here("functions", "financial_year.R"))

# Create data folder if this doesn't already exist
if(!("data" %in% fs::dir_ls())){fs::dir_create("data")}

if(!(glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}") %in% fs::dir_ls("data"))){
  fs::dir_create(glue("data/{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"))
}


### 2 - Read and clean collated file ----

pds <- 
  
  read_csv(glue("{filepath}dementia/03-Outputs/zNational/",
                     "{fy}-Q{qt}_national.csv"),
                col_types = cols(.default = "c")) %>%

  clean_names() %>%
  
  # Convert dates from character to date format
  mutate_at(vars(contains("date")), ~ lubridate::ymd(.)) %>%
  
  # Pad CHI Number to 10 digits
  mutate(chi_number = if_else(nchar(chi_number) == 9,
                              paste0("0", chi_number),
                              chi_number)) %>%
  
  # Replace word 'and' with ampersand
  mutate(health_board = str_replace(health_board, " and ", " & "))


### 3 - Save out error summary

err <- pds %>%
  filter(dementia_diagnosis_confirmed_date %within% 
           interval(start_date, end_date)) %>%
  mutate(health_board = if_else(is.na(health_board),
                                "Missing",
                                substring(health_board, 3))) %>%
  mutate(fy = financial_year(dementia_diagnosis_confirmed_date)) %>%
  group_by(fy, health_board, ijb) %>%
  summarise(total_errors     = sum(as.integer(error_flag)),
            diag_date_errors = sum(is.na(dementia_diagnosis_confirmed_date)),
            records          = n()) %>%
  ungroup()

err %>%
  bind_rows(err %>%
              group_by(fy, health_board = "Scotland", ijb = "Scotland") %>%
              summarise(total_errors     = sum(total_errors),
                        diag_date_errors = sum(diag_date_errors),
                        records          = sum(records)) %>%
              ungroup()) %>%
  arrange(fy, health_board, ijb) %>%
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


### 5 - Remove unusable records ----

pds %<>%
  
  # Remove records with missing diagnosis date
  filter(!is.na(dementia_diagnosis_confirmed_date)) %>%

  # Select records within reporting period only
  filter(dementia_diagnosis_confirmed_date %within% 
           interval(start_date, end_date))


### 6 - Remove duplicate records ----

pds %<>%

  # Add flag for GGC/H duplicates
  group_by(chi_number) %>%
  mutate(ggc_h_dupe = 
           if_else(n() > 1 &
                     all(health_board %in% c("G NHS Greater Glasgow & Clyde",
                                             "H NHS Highland")),
                   1,
                   0)) %>%
  ungroup()

# Save duplicate records
dupes <- 
  pds %>% 
  filter(ggc_h_dupe == 1) %>%
  select(-ggc_h_dupe) %>%
  mutate(dupe_kept = if_else(health_board == "G NHS Greater Glasgow & Clyde",
                             1, 0),
         dupe_type = "GGC/Highland SLA")

pds %<>%
  
  # Remove Highland duplicates
  filter(ggc_h_dupe == 0 | (ggc_h_dupe == 1 & health_board == "H NHS Highland")) %>%
  
  # Recode GGC duplicate as Argyll & Bute activity
  mutate(health_board = if_else(ggc_h_dupe == 1, "H NHS Highland", health_board),
         ijb = if_else(ggc_h_dupe == 1, "S37000004 Argyll and Bute", ijb))


# Save duplicates
write_csv(dupes,
          here("data", 
               glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
               glue("{fy}-{qt}_dupes.csv")))


### 7 - Save data ---

write_rds(pds, here("data", 
                    glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}"),
                    glue("{fy}-{qt}_clean-data.rds")))


### END OF SCRIPT ###