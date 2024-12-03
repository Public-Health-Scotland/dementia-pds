################################################################################
# Name of file - 01a_data-prep_population-files.R
# Data release - Dementia PDS Quarterly Management Reports
# Original Authors - Abram McCormick
# Original Date - December 2024
#
# Written/run on - R Posit
# Version of R - 4.1.2
#
# Description - Create population lookup files. ONLY RUN WHEN UPDATES ARE AVAILABLE.
################################################################################


### 1 - Load environment file ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Create population file from mid-year estimates ----

# Creating population for IJB

la_pop <- read_rds(glue("{cl_out}/lookups/Unicode/Populations/Estimates/",
                        "HSCP2019_pop_est_1981_2023.rds"))%>%
  filter(year >= 2016)

la_pop %<>%
  mutate(age_grp = 
           case_when(
             age < 0 | is.na(age) ~ "Unknown",
             age %in% 0:59 ~ "59 and Under",
             age %in% 60:64 ~ "60 to 64",
             age %in% 65:69 ~ "65 to 69",
             age %in% 70:74 ~ "70 to 74",
             age %in% 75:79 ~ "75 to 79",
             age %in% 80:84 ~ "80 to 84",
             age %in% 85:89 ~ "85 to 89",
             age >= 90      ~ "90+"
           )) %>% 
  mutate(sex = 
           case_when(
             sex == 1 ~ "01 Male",
             sex == 2 ~ "02 Female"
           ))

# Aggregate by year, LA, age group and gender

la_pop_data <-
  
  bind_rows(
    
    la_pop %>%
      group_by(geog=hscp2019name, year, age_grp, sex) %>%
      summarise(pop_est=sum(pop), .groups ="drop"),
    
    la_pop %>%
      group_by(geog=hscp2019name, year, age_grp = "All", sex) %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog="Scotland", year, age_grp, sex) %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog="Scotland", year, age_grp = "All", sex) %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog=hscp2019name, year, age_grp, sex = "All") %>%
      summarise(pop_est=sum(pop), .groups ="drop"),
    
    la_pop %>%
      group_by(geog=hscp2019name, year, age_grp = "All", sex = "All") %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog="Scotland", year, age_grp, sex = "All") %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    la_pop %>%
      group_by(geog="Scotland", year, age_grp = "All", sex = "All") %>%
      summarise(pop_est=sum(pop), .groups = "drop")
  )

la_pop_data %<>% 
  mutate (geog =
            case_when(
              str_detect(geog, "City of Edinburgh") ~ "Edinburgh City",
              str_detect(geog, "Na h-Eileanan Siar") ~ "Western Isles",
              TRUE ~ geog
            ))

# Creating population for HB

hb_pop <- read_rds(glue("{cl_out}/lookups/Unicode/Populations/Estimates/",
                        "HB2019_pop_est_1981_2023.rds"))%>%
  filter(year >= 2016)

hb_pop %<>%
  mutate(age_grp = 
           case_when(
             age < 0 | is.na(age) ~ "Unknown",
             age %in% 0:59 ~ "59 and Under",
             age %in% 60:64 ~ "60 to 64",
             age %in% 65:69 ~ "65 to 69",
             age %in% 70:74 ~ "70 to 74",
             age %in% 75:79 ~ "75 to 79",
             age %in% 80:84 ~ "80 to 84",
             age %in% 85:89 ~ "85 to 89",
             age >= 90      ~ "90+"
           )) %>% 
  mutate(sex = 
           case_when(
             sex == 1 ~ "01 Male",
             sex == 2 ~ "02 Female"
           ))

# Aggregate by year, hb, age group and gender

hb_pop_data <- 
  
  bind_rows(
    
    hb_pop %>% 
      group_by(geog=hb2019name, year, age_grp, sex) %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog=hb2019name, year, age_grp = "All", sex) %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog=hb2019name, year, age_grp, sex = "All") %>%
      summarise(pop_est=sum(pop), .groups = "drop"),
    
    hb_pop %>% 
      group_by(geog=hb2019name, year, age_grp = "All", sex = "All") %>%
      summarise(pop_est=sum(pop), .groups = "drop") 
  )

hb_pop_data %<>% 
  mutate (geog =
            case_when(
              str_detect(geog, "NHS Ayrshire and Arran") ~ "NHS Ayrshire & Arran",
              str_detect(geog, "NHS Dumfries and Galloway") ~ "NHS Dumfries & Galloway",
              str_detect(geog, "NHS Greater Glasgow and Clyde") ~ "NHS Greater Glasgow & Clyde",
              TRUE ~ geog
            ))

# Merge LA and HB pops
pop_data <-bind_rows(la_pop_data, hb_pop_data)


# Add 2024 populations from 2023
pop_data %<>%
  
  bind_rows(
    
    pop_data %>%
      filter(year == 2023) %>%
      mutate(year = 2024)
  ) 

#check
tabyl(pop_data$geog)

pop_data %>% 
  write_file(path = "//conf/dementia/A&I/Outputs/management-report/lookups/pop_data.rds")
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.



### 3 - read in simd pop file ---- 

simd_pop_16_21 <- read_csv("//conf/dementia/A&I/PDS Development/data/simd_pop_16-21.csv") %>% 
  select(-Total)

simd_pop_16_21 %<>% pivot_longer(cols = 5:95,
                                 names_to = "age",
                                 values_to = "pop") 

simd_pop_16_21 %<>% mutate(simd = case_when(
  decile %in% 1:2 ~ "1 - Most Deprived",
  decile %in% 3:4 ~ "2",
  decile %in% 5:6 ~ "3",
  decile %in% 7:8 ~ "4",
  decile %in% 9:10 ~ "5 - Least Deprived"
), .before = decile)


simd_pop_data <- simd_pop_16_21 %>% group_by(year, geog, simd, sex, age) %>% 
  summarise(pop = sum(pop), .groups = "drop")


simd_pop_data %<>% mutate(age_grp = 
                            case_when(
                              age < 0 | is.na(age) ~ "Unknown",
                              age %in% 0:59 ~ "59 and Under",
                              age %in% 60:64 ~ "60 to 64",
                              age %in% 65:69 ~ "65 to 69",
                              age %in% 70:74 ~ "70 to 74",
                              age %in% 75:79 ~ "75 to 79",
                              age %in% 80:84 ~ "80 to 84",
                              age %in% 85:89 ~ "85 to 89",
                              age >= 90      ~ "90+"
                            )) %>% 
  mutate(sex = 
           case_when(
             sex == "Males" ~ "01 Male",
             sex == "Females" ~ "02 Female",
             sex == "All" ~ "All"
           ))

simd_pop_data %<>% group_by(geog, year, age_grp, sex, simd) %>% summarise(pop = sum(pop), .groups = "drop")

simd_pop_summary <- bind_rows(simd_pop_data, simd_pop_data %>% group_by(geog, year, age_grp = "All", sex, simd) %>% summarise(pop = sum(pop), .groups = "drop"))

simd_pop_summary %<>% 
  mutate (geog =
            case_when(
              str_detect(geog, "NHS Ayrshire and Arran") ~ "NHS Ayrshire & Arran",
              str_detect(geog, "NHS Dumfries and Galloway") ~ "NHS Dumfries & Galloway",
              str_detect(geog, "NHS Greater Glasgow and Clyde") ~ "NHS Greater Glasgow & Clyde",
              TRUE ~ geog
            ))

simd_pop_summary%<>% mutate (geog =
                               case_when(
                                 str_detect(geog, "City of Edinburgh") ~ "Edinburgh City",
                                 str_detect(geog, "Na h-Eileanan Siar") ~ "Western Isles",
                                 TRUE ~ geog
                               ))

simd_pop_summary %<>% 
  bind_rows(
    simd_pop_summary %>%
      filter(year == 2021) %>%
      mutate(year = 2022),
    
    simd_pop_summary %>%
      filter(year == 2021) %>%
      mutate(year = 2023),
    
    simd_pop_summary %>%
      filter(year == 2021) %>%
      mutate(year = 2024))


cl_stirling <- simd_pop_summary %>% filter(geog == "Clackmannanshire" | geog == "Stirling") %>% mutate(ijb = "Clackmannanshire and Stirling", .before = everything()) 
cls_pop<-cl_stirling %>% group_by(ijb, year, age_grp, sex, simd) %>% summarise(pop = sum(pop)) %>% rename("geog" = "ijb")

simd_pop_summary %<>% filter(geog != "Clackmannanshire" & geog != "Stirling") 

simd_pop_data <- bind_rows(simd_pop_summary, cls_pop)


simd_pop_data %>% 
  write_file(path = "//conf/dementia/A&I/Outputs/management-report/lookups/simd_pop_data.rds")
0 # this zero stops script from running IF write_file is overwriting an existing file, re-run the section without this line and enter 1 in the console, when prompted, to overwrite file.

### END OF SCRIPT

