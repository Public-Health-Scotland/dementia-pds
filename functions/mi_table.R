mi_table <- function(
    data, 
    table, 
    geog_filter = c("scot", "ijb", "health_board"), 
    measure = NA,
    years = all_years,
    is_complete_year = complete_year,
    demog = "none",
    dropdown_grp = "grp",
    height = 660){
  
  if (!table %in% c("error", "submission", "completeness")){
    data <- data %>% 
      format_numbers() %>%
      rename("age" = "age_grp_2") %>%
      filter(
        .data$demog == .env$demog,
        name %in% geog_filter,
        fy %in% years) %>%
      arrange(desc(name), geog)
  }

  if (demog != "none"){
    data <- data %>%
      filter(!.data[[demog]] %in% c("Unknown", "98 Not Specified", "99 Not Known"))
    demog_name <- switch(
      demog,
      "age" = "Age Group",
      "sex" = "Gender",
      "simd" = "Deprivation Quintile"
    )
  } 
  
  geog_name <- case_when(
    !"health_board" %in% geog_filter ~ "Integration Authority Area",
    !"ijb" %in% geog_filter ~ "Health Board",
    TRUE ~ "Integration Authority Area/Health Board"
  )
  
  ##############################################################################.
  # Subpage: Scotland ----
  ##############################################################################.
  
  # Number of Individuals relating to LDP Standard
  if(table == "scot"){
    data %>% 
      select(name, " " = geog, referrals, met, not_met, ongoing, exempt, perc_met) %>%
      {if (isFALSE(is_complete_year)) select(., -perc_met) else .} %>%
      rename_columns() %>% 
      format_static_table(type = "scot", is_complete_year = is_complete_year)
  }
  
  ################################################################################.
  # Subpage: Health Boards ----
  ################################################################################.
  
  # Number of Individuals relating to LDP Standard
  else if (table == "hb"){
    data %>% 
      arrange(name) %>%
      select(name, x = geog, referrals, met, not_met, ongoing, exempt, perc_met) %>% 
      {if (isFALSE(is_complete_year)) select(., -perc_met) else .} %>%
      rename_columns() %>% 
      pivot_longer(names_to = " ", cols = -x) %>%
      pivot_wider(names_from = x, values_from = value) %>% 
      format_static_table(type = "hb", is_complete_year = is_complete_year)
  }
  
  ##############################################################################.
  # Subpage: All Referrals - Trends & Rates
  ##############################################################################.
  
  # Number of Individuals Diagnosed and Referred for PDS/
  # Number of Individuals Diagnosed and Referred for PDS per 10,000 Population (65+)/
  # Percentage of estimated diagnoses referred to PDS/
  # Percentage receiving one year’s PDS support
  else if (table == "trend"){
    data %>% 
      select(geog, name, fy, all_of(measure)) %>% 
      pivot_wider(names_from = "fy", values_from = all_of(measure)) %>%
      rename_columns() %>% 
      format_static_table(type = "trend")
  }
  
  ##############################################################################.
  # Subpage: Demographics
  ##############################################################################.

  # Number of people referred to PDS per 10,000 population by Demographic/
  # Percentage of referrals who received a minimum of one year’s PDS by Demographic
  else if (table == "demog"){
    data <- data %>% 
      mutate(key = paste0(fy, geog))
    
    if (measure == "rate"){
      data <- data %>% 
        select(key, geog, name, all_of(demog), rate) %>% 
        pivot_wider(names_from = all_of(demog), values_from = "rate") %>%
        rename_columns()
    
    } else if (measure == "perc_met"){
      data <- data %>%
        select(key, fy, geog, name, all_of(demog), referrals, perc_met) %>% 
        rename_columns() %>%
        rename("Number of Referrals" = "Total Referrals") %>%
        pivot_longer(names_to = " ", cols = c("Number of Referrals", "% Met Standard/Exempt")) %>%
        pivot_wider(names_from = all_of(demog_name), values_from = "value") %>%
        mutate(
          !!geog_name := ifelse(` ` == "Number of Referrals", as.character(.data[[geog_name]]),""),
          " " = if_else(` ` == "Number of Referrals" & !fy %in% finalised_years, "Number of Referrals (excludes PDS Ongoing)", ` `)) %>%
        select(-fy)
    }
    data %>% 
      format_dynamic_table(type = "outcome", dropdown_grp = dropdown_grp, height = height)
  }
  
  else if (table == "demog_trend"){
    data <- data %>% 
      mutate(key = geog)
    
    if (measure == "rate"){
      data <- data %>%
        select(key, fy, name, .data[[demog]], rate) %>%
        pivot_wider(names_from = "fy", values_from = "rate") %>%
        rename_columns()
      
    } else if (measure == "perc_met"){
      data <- data %>%
        select(key, fy, name, .data[[demog]], referrals, perc_met) %>%
        rename_columns() %>%
        rename("Number of Referrals (excludes PDS Ongoing)" = "Total Referrals") %>%
        pivot_longer(names_to = " ", cols = c("Number of Referrals (excludes PDS Ongoing)", "% Met Standard/Exempt")) %>%
        pivot_wider(names_from = "fy", values_from = "value") %>%
        mutate(!!demog_name := ifelse(` ` == "Number of Referrals (excludes PDS Ongoing)", .data[[demog_name]],""))
    }
    data %>% format_dynamic_table(type = "trend", dropdown_grp = dropdown_grp, height = height)
  }
  
  ##############################################################################.
  # Subpage: Pathways
  ##############################################################################.
  
  # Average (median) days from diagnosis to first contact by PDS practitioner/
  # Pathways from diagnosis to first contact by PDS practitioner/
  # Individuals who did not receive a minimum of one year’s PDS/
  # Individuals where LDP Standard cannot yet be determined/
  # Individuals exempt from LDP Standard/
  # Referrals by PDS Uptake Decision/
  # Individuals referred to PDS not contacted within 12 months of diagnosis
  else if (table == "pathway"){
    if (measure == "wait_trend"){
      data <- data %>% 
        mutate(key = "key") %>%
        select(key, geog, name, fy, diagnosis_to_contact) %>%
        pivot_wider(names_from = fy, values_from = diagnosis_to_contact)
    } else {
      colselect <- switch(
        measure,
        "wait" = c("perc_contacted", "diagnosis_to_contact"),
        "wait_breakdown" = c(
          "diagnosis_to_referral", "perc_allocated", "referral_to_allocation",
          "perc_contacted", "allocation_to_contact", "diagnosis_to_contact"),
        "not_met" = c("perc_not_met", "not_met", grep("fail", names(data), value = TRUE)),
        "ongoing" = grep("ongoing", names(data), value = TRUE),
        "exempt" = c("perc_exempt", "exempt"),
        "uptake" = c("perc_uptake_decision", "perc_accepted"),
        "no_contact" = c(
          "no_contact_12", "perc_no_contact_12", "termination_before_contact",
          "perc_termination_before_contact")
      )

      data <- data %>% 
        mutate(key = fy) %>%
        select(key, geog, name, referrals, all_of(colselect))
    }
    
    data %>%
      rename_columns() %>%
      format_dynamic_table(type = "pathway", measure = measure, dropdown_grp = dropdown_grp, height = height)
  }
  
  ##############################################################################.
  # Subpage: Data Quality
  ##############################################################################.
  
  else if (table == "error"){
    create_error_table(data) %>%
      format_static_table(type = "error")
  }
  
  else if (table == "completeness"){
    data %>%
      filter(demog == "none") %>%
      create_completeness_table(measure) %>%
      format_dynamic_table(type = "completeness", height = height)
  }
  
  else if (table == "submission"){
    create_records_summary_table(data) %>%
      format_static_table(type = "submission")
  }
}
