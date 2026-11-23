format_numbers <- function(data){
  rate_cols <- str_subset(names(data), "rate")
  time_cols <- str_subset(names(data), "_to_")
  perc_cols <- str_subset(names(data), "perc|fail_|ongoing_|exempt_")
  num_cols <- setdiff(names(data), c(rate_cols, time_cols, perc_cols, names(data)[!sapply(data, is.numeric)]))
  
  data %>%
    mutate(
      across(any_of(perc_cols), ~ ifelse(!is.na(.x), paste0(round(.x, 1), "%"), "-")),
      across(any_of(num_cols), ~ ifelse(!is.na(.x), format(.x, big.mark = ","), "-")),
      across(any_of(rate_cols), ~ ifelse(!is.na(.x), round(.x, 1), "-")),
      across(any_of(time_cols), ~ ifelse(!is.na(.x), as.character(.x), "-"))
    )
}     

# # Rates
# standardised_rate
# rate
# exp_rate
# 
# # %
# perc_met
# perc_not_met
# perc_ongoing
# perc_exempt
# perc_allocated
# perc_contacted
# perc_uptake_decision
# perc_accepted
# perc_no_contact_12
# perc_termination_before_contact
# fail_term_before_11
# fail_start_over_12
# fail_term_before_contact
# fail_not_started_over_12
# ongoing_under_12
# ongoing_not_started_under_12
# exempt_no_engagement
# exempt_moved
# exempt_term_early
# exempt_died
# 
# accom_unknown
# stage_unknown
# stage_ytbd
# model_unknown
# model_ytbd
# age_unknown
# sex_unknown
# simd_unknown
# 
# # Time
# diagnosis_to_referral
# referral_to_allocation
# allocation_to_contact
# diagnosis_to_contact
# 
# # Count
# referrals
# met
# not_met
# ongoing
# exempt
# uptake_decision
# no_contact_12
# termination_before_contact
# referrals_over_65
# diagnoses
