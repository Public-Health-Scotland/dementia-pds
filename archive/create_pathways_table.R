create_pathways_table <- function(data, measure, geog_filter = c("scot", "ijb", "hb"), group = NULL){
  
  geog_name <- case_when(
    "ijb" %in% geog_filter & "health_board" %in% geog_filter ~ "Integration Authority Area/Health Board",
    "ijb" %in% geog_filter ~ "Integration Authority Area",
    "health_board" %in% geog_filter ~ "Health Board")
  
  data <- data %>%  
    filter(name %in% geog_filter) %>%
    mutate(
      referrals = ifelse(!is.na(referrals), format(referrals, big.mark = ","), "-"),
      across(contains("perc"), ~ ifelse(!is.na(.x), paste0(round(.x, 1), "%"), "-")),
      across(contains("_to_"), ~ ifelse(!is.na(.x), as.character(.x), "-"))) %>%
    arrange(desc(name)) %>%
    select(-name)
  
  page_length <- length(unique(data$geog))
  
  if (measure == "wait"){
    table_data <- data %>%
      select(
        fy,
        !!geog_name := geog,
        "Number of People Referred to PDS" = referrals,
        "% of Referrals contacted by PDS practitioner" = perc_contacted,
        "Average (median) days from diagnosis to first contact" = diagnosis_to_contact)
  }
  
  if (measure == "wait_breakdown"){
    table_data <- data %>%
      select(
        fy,
        !!geog_name := geog,
        "Number of People Referred to PDS" = referrals,
        "Average (median) days from diagnosis to PDS referral" = diagnosis_to_referral,	
        "% of Referrals allocated to PDS practitioner" = perc_allocated,
        "Average (median) days from referral to allocation of PDS practitioner" = referral_to_allocation,
        "% of Referrals contacted by PDS practitioner" = perc_contacted,
        "Average (median) days from allocation to first contact with PDS practitioner" = allocation_to_contact,
        "Average (median) days from diagnosis to first contact" = diagnosis_to_contact)
  }
  
  if (measure == "exempt"){
    table_data <- data %>%
      mutate(exempt = ifelse(!is.na(exempt), format(exempt, big.mark = ","), "-")) %>%
      select(
        fy,
        !!geog_name := geog,
        "Total referrals" = referrals,
        "% of Total referrals Exempt" = perc_exempt,	
        "Number of referrals Exempt" = exempt)
  }
  
  if (measure == "uptake"){
    table_data <- data %>%
      select(
        fy,
        !!geog_name := geog,
        "Number of People Referred to PDS" = referrals,
        "% of referrals where Uptake Decision is Recorded" = perc_uptake_decision,	
        "	% Accepted (includes those initially declined)" = perc_accepted)
  }
  
  if (measure == "no_contact"){
    table_data <- data %>%
      mutate(across(
        c("no_contact_12", "termination_before_contact"), 
        ~ ifelse(!is.na(.x), format(.x, big.mark = ","), "-"))) %>%
      select(
        fy,
        !!geog_name := geog,
        "Number of People Referred to PDS" = referrals,
        "% of total referrals not contacted within 12 months of diagnosis" = perc_no_contact_12,	
        "Number of referrals not contacted within 12 months of diagnosis" = no_contact_12,
        "% of total referrals whose PDS ended with no contact" = perc_termination_before_contact,
        "Number of referrals whose PDS ended with no contact" = termination_before_contact)
  }

  if (measure == "not_met"){
    columns <- c(
      "PDS started more than 12 months after diagnosis",	
      "PDS not started and more than 12 months since diagnosis",
      "PDS terminated less than 11 months after first contact",
      "PDS terminated before first contact")
    table_data <- data %>%
      mutate(
        not_met = ifelse(!is.na(not_met), format(not_met, big.mark = ","), "-"),
        across(all_of(columns), ~ ifelse(!is.na(.x), paste0(round(.x, 1), "%"), "-"))) %>%
      select(fy, geog, referrals, perc_not_met, not_met, all_of(columns))
    colnames(table_data) <- c(
      "fy", 
      geog_name, 
      "Total referrals",
      "% of Total referrals Standard Not Met",
      "Standard Not Met",
      "First contact occurred more than 12 months after diagnosis",	
      "No contact and more than 12 months since diagnosis",
      "PDS terminated less than 11 months after first contact",
      "PDS terminated without contact")
    breakdown_header <- "% of Standard Not Met"
  }
  
  if (measure == "ongoing"){
    columns <- c(
      "PDS not started and less than 12 months since diagnosis",	
      "Still receiving PDS and less than 12 months since first contact")
    table_data <- data %>%
      mutate(
        ongoing = ifelse(!is.na(ongoing), format(ongoing, big.mark = ","), "-"),
        across(all_of(columns), ~ ifelse(!is.na(.x), paste0(round(.x, 1), "%"), "-"))) %>%
      select(fy, geog, referrals, perc_ongoing, ongoing, all_of(columns))
    colnames(table_data) <- c(
      "fy", 
      geog_name, 
      "Total referrals",
      "% of Total referrals PDS Ongoing",
      "PDS Ongoing",
      "Less than 12 months since diagnosis and PDS not yet started",	
      "Still receiving PDS and less than 12 months since first contact"
    )
    breakdown_header <- "% of PDS Ongoing"
  }
  
  right_align <- length(names(table_data)) - 1
  
  if (measure %in% c("not_met", "ongoing")){
    table_headers <- htmltools::withTags(table(
      class = 'display', thead(tr( 
        th(rowspan = 2, geog_name),
        th(rowspan = 2, " "),
        th(rowspan = 2, names(table_data)[3]),
        th(rowspan = 2, names(table_data)[4]),
        th(rowspan = 2, names(table_data)[5]),
        th(colspan = ncol(table_data) - 5,
           breakdown_header,
           style = css(textAlign="center"))),
        tr(lapply(names(table_data)[6:ncol(table_data)], th)))))
  }
  
  table_data <- SharedData$new(
    table_data,
    key = ~fy, 
    group = group)
  
  datatable_args <- list(
    data = table_data,
    rownames = FALSE, 
    extensions = "FixedColumns",
    options = list(
      pageLength = page_length,
      scrollX = FALSE,
      fixedColumns = list(leftColumns = 1),
      scrollY = TRUE,
      dom = "t",
      ordering = FALSE,
      columnDefs = list(
        list(visible = FALSE, targets = 0),
        list(className = 'dt-right', targets = 2:right_align))))
  
  if (measure %in% c("not_met", "ongoing")){
    datatable_args$container <- table_headers
  }
  
  if (measure %in% c("wait", "wait_breakdown")){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "A dash (-) indicates that due to data quality issues, or lack of data, this measure could not be calculated.")
  }
  
  if (measure == "not_met"){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "PDS terminated without contact refers to those with no contact date recorded and a termination date recorded at any time after diagnosis, therefore this may include referrals with a termination date more than 12 months after diagnosis.")
  }
  
  if (measure == "no_contact"){
    datatable_args$caption <- htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left; color:black;',
      "Referrals not contacted within 12 months of diagnosis includes records with no contact date and those with a contact date more than 12 months after diagnosis, but does not include those with a termination date.", 
      htmltools::tags$br(), 
      "Number of referrals whose PDS ended with no contact refers to those with no contact date recorded and a termination date recorded at any time after diagnosis, therefore this may include referrals with a termination date more than 12 months after diagnosis.")
  }

  dt <- do.call(DT::datatable, datatable_args) %>%
    DT::formatStyle(1, fontWeight = "bold")
  
  if (measure %in% c("not_met", "ongoing", "no_contact")){
    dt <- dt %>%
      DT::formatStyle(5, `border-right` = "solid 1px")
  }
  
  if (measure == "wait_breakdown"){
    dt <- dt %>%
      DT::formatStyle(right_align, fontWeight = "bold", backgroundColor = "#E1C7DF")
  } 
  
  bscols(widths = 12, div(style = css(width="97%", height="660px", margin="20px"), dt))
}
