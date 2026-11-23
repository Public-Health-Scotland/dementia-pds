rename_columns <- function(data){
  
  geog_name <- case_when(
    "ijb" %in% unique(data$name) & "health_board" %in% unique(data$name) ~ "Integration Authority Area/Health Board",
    "ijb" %in% unique(data$name) ~ "Integration Authority Area",
    "health_board" %in% unique(data$name) ~ "Health Board"
  )
  
  rename_map <- c(
    
    # Geography
    setNames("geog", geog_name),
    
    # Outcomes
    "Total Referrals" = "referrals",
    "Standard Met" = "met",
    "Standard Not Met" = "not_met",
    "PDS Ongoing" = "ongoing",
    "Exempt" = "exempt",
    "% Met Standard/Exempt" = "perc_met",
    "Number of Referrals (excludes PDS Ongoing)" = "refs",
    
    # Demographics
    "Age Group" = "age_grp_2",
    "Age Group" = "age",
    "Gender" = "sex",
    "Deprivation Quintile" = "simd",
    
    # Waiting times
    "Number of People Referred to PDS" = "referrals",
    "% of Referrals contacted by PDS practitioner" = "perc_contacted",
    "Average (median) days from diagnosis to first contact" = "diagnosis_to_contact",
    "Average (median) days from diagnosis to PDS referral" = "diagnosis_to_referral",
    "% of Referrals allocated to PDS practitioner" = "perc_allocated",
    "Average (median) days from referral to allocation of PDS practitioner" = "referral_to_allocation",
    "Average (median) days from allocation to first contact with PDS practitioner" = "allocation_to_contact",
    
    # Exempt
    "% of Total referrals Exempt" = "perc_exempt",
    "Number of referrals Exempt" = "exempt",
    
    # Uptake
    "% of referrals where Uptake Decision is Recorded" = "perc_uptake_decision",
    "% Accepted (includes those initially declined)" = "perc_accepted",
    
    # No contact
    "% of total referrals not contacted within 12 months of diagnosis" = "perc_no_contact_12",
    "Number of referrals not contacted within 12 months of diagnosis" = "no_contact_12",
    "% of total referrals whose PDS ended with no contact" = "perc_termination_before_contact",
    "Number of referrals whose PDS ended with no contact" = "termination_before_contact",
    
    # Standard not met
    "% of Total referrals Standard Not Met" = "perc_not_met",
    "First contact occurred more than 12 months after diagnosis" = "fail_start_over_12",
    "No contact and more than 12 months since diagnosis" = "fail_not_started_over_12",
    "PDS terminated less than 11 months after first contact" = "fail_term_before_11",
    "PDS terminated without contact" = "fail_term_before_contact",
    
    # Ongoing
    "% of Total referrals PDS Ongoing" = "perc_ongoing",
    "Less than 12 months since diagnosis and PDS not yet started" = "ongoing_not_started_under_12",
    "Still receiving PDS and less than 12 months since first contact" = "ongoing_under_12"
  )
  
  data %>%
    rename(any_of(rename_map)) %>%
    select(-name)
}
