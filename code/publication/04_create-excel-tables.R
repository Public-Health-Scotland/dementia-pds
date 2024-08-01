#########################################################################
# Name of file - 04_create-excel-tables.R
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

source(here::here("code", "publication", "00_setup-pub-environment.R"))


### 2 - Load data ----

pds <- read_rds(get_pub_data_path())

expected <- read_csv(get_exp_diagnoses_path()) %>%
  select(-health_board)


### 3 - Restructure data ----

excel_data <-
  
  bind_rows(
    
    # Health Board
    pds %>%
      group_by(fy, category = "geog", category_split = health_board) %>%
      summarise(across(complete:denominator, sum), .groups = "drop"),

    # IJB
    pds %>%
      group_by(fy, category = "geog", category_split = ijb) %>%
      summarise(across(complete:denominator, sum), .groups = "drop"),
    
    # Scotland
    pds %>% 
      group_by(fy, category = "geog", category_split = "Scotland") %>%
      summarise(across(complete:denominator, sum), .groups = "drop"),
    
    # Age Group
    pds %>% 
      group_by(fy, category = "age", category_split = age_grp) %>%
      summarise(across(complete:denominator, sum), .groups = "drop"),
    
    # SIMD
    pds %>% 
      group_by(fy, category = "simd", category_split = simd) %>%
      summarise(across(complete:denominator, sum), .groups = "drop")
    
  ) %>%

  # Add rate column
  mutate(rate = numerator / denominator) %>%
  select(-c(numerator:denominator)) %>%
  
  # Add expected diagnoses
  left_join(expected, by = c("fy", "category_split" = "health_board_label")) %>%
  mutate(exp_rate = referrals / diagnoses) %>%
  
  # Add lookup column
  mutate(lookup = paste0(fy, category, category_split), .before = everything())


### 4 - Save data to excel template ----

wb <- loadWorkbook(get_excel_template_path())

writeData(wb,
          "data",
          excel_data,
          startCol = 1,
          name = "data")

fy_lookup <-
  tibble(fy_in_pub) %>%
  mutate(n = row_number(), .before = everything()) %>%
  mutate(sup = case_when(
    n == max(n) ~ paste0(fy_in_pub, "ᴾ"),
    n == max(n) - 1 ~ paste0(fy_in_pub, "ᴿ"),
    TRUE ~ fy_in_pub
  ))

# Add some lookup values to calculation tab
writeData(
    wb,
    "calculation",
    fy_lookup,
    startCol = "A",
    startRow = 2,
    colNames = FALSE,
    name = "fy"
  )

writeData(
  wb,
  "calculation",
  fy_lookup$sup,
  startCol = "D",
  startRow = 2,
  colNames = FALSE,
  name = "fy_dropdown"
)

writeData(
  wb,
  "calculation",
  as.character(glue_collapse(fy_in_pub, sep = ", ", last = " and ")),
  startCol = "G",
  startRow = 2
)

writeData(
  wb,
  "calculation",
  format(end_date, "'%d %B %Y"),
  startCol = "G",
  startRow = 3
)

writeData(
  wb,
  "calculation",
  max(fy_in_pub),
  startCol = "G",
  startRow = 4
)

# Add publication link to Notes page
link <- 
  paste0("https://publichealthscotland.scot/publications/",
         "dementia-post-diagnostic-support/dementia-post-",
         "diagnostic-support-local-delivery-plan-standard-figures-to-", 
         (latest_fy))

# names(link) <- "See Appendix 2 of the full report for more information."
class(link) <- "hyperlink"

writeData(wb, 
          "Notes", 
          startCol = "C",
          startRow = 21,
          x = link)

addStyle(wb = wb, 
         sheet = "Notes", 
         cols = "C",
         rows = 21,
         style = createStyle(fontName = "Arial", fontColour = "#0000ff",
                             textDecoration = "underline"))

# Add text re estimates used for each year
estimates_year <- paste0(
  "Estimates are used as follows: ",
  glue_collapse(
    glue("calendar year {str_sub(fy_in_pub, 1, 4)} estimates for {fy_in_pub}"),
    sep = ", ",
    last = " and "
  ),
  "."
)

writeData(
  wb,
  "Tab 6",
  estimates_year,
  startCol = "B",
  startRow = 31
)

writeData(
  wb,
  "Tab 7",
  estimates_year,
  startCol = "B",
  startRow = 29
)

# Add embargo text
writeData(wb,
          "Notes",
          paste("RESTRICTED STATISTICS: embargoed to 09:30", 
                format(pub_date, "%d/%m/%Y")),
          startCol = "B",
          startRow = 1)

# Hide data sheets and calculation sheet
sheetVisibility(wb)[10:11] <- "hidden"

saveWorkbook(wb,
             get_pub_output_path(output_name = "excel_tables"),
             overwrite = TRUE)


### END OF SCRIPT ###