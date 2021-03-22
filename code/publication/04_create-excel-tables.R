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

source(here::here("code", "publication", "00_setup-pub-environment.R"))


### 2 - Load data ----

pds <-
  read_rds(
    here("data", "publication", glue("{pub_date}_pub-data.rds"))
  ) %>%
  filter(ijb != "Unknown")

expected <-
  read_csv(here("reference-files", "expected-diagnoses.csv")) %>%
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

wb <- loadWorkbook(here("reference-files",
                        "excel-template.xlsx"))

writeData(wb,
          "data",
          excel_data,
          startCol = 1)

# Add some lookup values to calculation tab
writeData(
    wb,
    "calculation",
    tibble(fy_in_pub) %>%
      mutate(n = row_number(), .before = everything()),
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  )

writeData(
  wb,
  "calculation",
  as.character(glue_collapse(fy_in_pub, sep = ", ", last = " and ")),
  startCol = "F",
  startRow = 5
)

writeData(
  wb,
  "calculation",
  format(end_date, "'%d %B %Y"),
  startCol = "F",
  startRow = 6
)

writeData(
  wb,
  "calculation",
  max(fy_in_pub),
  startCol = "F",
  startRow = 8
)

writeData(
  wb,
  "calculation",
  nth(fy_in_pub, -2),
  startCol = "F",
  startRow = 9
)

# Add publication link to Notes page
link <- 
  paste0("https://beta.isdscotland.org/find-publications-and-data/",
         "conditions-and-diseases/dementia/dementia-post-diagnostic-support/",
         format(pub_date, "%e-%B-%Y") %>% str_trim())
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
             here("publication", "output", pub_date, 
                  glue("{pub_date}_dementia-pds_excel-tables.xlsx")),
             overwrite = TRUE)


### END OF SCRIPT ###