create_measures_table <- function(pds, exp_df, demographic = c()){
  measures_df <- pds %>%
    { if ("age_grp_2" %in% demographic) filter(., age_grp_2 != "Unknown") else . } %>%
    { if ("sex" %in% demographic) filter(., !sex %in% c("98 Not Specified", "99 Not Known")) else . } %>%
    { if ("simd" %in% demographic) filter(., simd != "Unknown") else . } %>%
    # Group by health board/ijb/scotland (geog/name/board), year (fy) and demographics(age_grp_2/sex/simd)
    group_by(geog, fy, name, board, across(any_of(demographic))) %>%
    summarise(
      # Referrals
      referrals = n(),
      # LDP Standard
      met                                                               = sum(str_detect(ldp, "complete"), na.rm = TRUE),
      not_met                                                           = sum(str_detect(ldp, "fail"), na.rm = TRUE),
      ongoing                                                           = sum(str_detect(ldp, "ongoing"), na.rm = TRUE),
      exempt                                                            = sum(str_detect(ldp, "exempt"), na.rm = TRUE),
      perc_met                                                          = round((met + exempt)/(met + exempt + not_met) * 100, 1),
      perc_not_met                                                      = round(not_met/referrals * 100, 1),
      perc_ongoing                                                      = round(ongoing/referrals * 100, 1),
      perc_exempt                                                       = round(exempt/referrals * 100, 1),
      `PDS terminated less than 11 months after first contact`          = round(sum(ldp == "fail - PDS terminated less than 11 months after first contact", na.rm = TRUE)/not_met * 100, 1),
      `PDS started more than 12 months after diagnosis`                 = round(sum(ldp == "fail - PDS started more than 12 months after diagnosis", na.rm = TRUE)/not_met * 100, 1),
      `PDS terminated before first contact`                             = round(sum(ldp == "fail - PDS terminated before first contact", na.rm = TRUE)/not_met * 100, 1),
      `PDS not started and more than 12 months since diagnosis`         = round(sum(ldp == "fail - PDS not started and more than 12 months since diagnosis", na.rm = TRUE)/not_met * 100, 1),
      `Still receiving PDS and less than 12 months since first contact` = round(sum(ldp == "ongoing - Still receiving PDS and less than 12 months since first contact", na.rm = TRUE)/ongoing * 100, 1),
      `PDS not started and less than 12 months since diagnosis`         = round(sum(ldp == "ongoing - PDS not started and less than 12 months since diagnosis", na.rm = TRUE)/ongoing * 100, 1),
      `Service user no longer able to engage in PDS`                    = round(sum(ldp == "exempt - 06 Service user no longer able to engage in PDS", na.rm = TRUE)/exempt * 100, 1),
      `Service user has moved to a different Health Board area`         = round(sum(ldp == "exempt - 04 Service user has moved to a different Health Board area", na.rm = TRUE)/exempt * 100, 1),
      `Service user has terminated PDS early/refused`                   = round(sum(ldp == "exempt - 05 Service user has terminated PDS early/refused", na.rm = TRUE)/exempt * 100, 1),
      `Service user has died`                                           = round(sum(ldp == "exempt - 03 Service user has died", na.rm = TRUE)/exempt * 100, 1),
      # Waiting Times
      perc_allocated         = round(sum(!is.na(initial_pds_practitioner_allocation_date), na.rm = TRUE)/referrals * 100, 1),
      perc_contacted         = round(sum(!is.na(date_of_initial_first_contact), na.rm = TRUE)/referrals * 100, 1),
      diagnosis_to_referral  = median(date_pds_referral_received - dementia_diagnosis_confirmed_date, na.rm = TRUE),
      referral_to_allocation = median(initial_pds_practitioner_allocation_date - date_pds_referral_received, na.rm = TRUE),
      allocation_to_contact  = median(date_of_initial_first_contact - initial_pds_practitioner_allocation_date, na.rm = TRUE),
      diagnosis_to_contact   = median(date_of_initial_first_contact - dementia_diagnosis_confirmed_date, na.rm = TRUE),
      # Uptake Decision
      uptake_decision      = sum(!is.na(pds_uptake_decision), na.rm = TRUE),
      perc_uptake_decision = round(uptake_decision/referrals * 100, 1),
      perc_accepted        = round(sum(pds_uptake_decision %in% c("01 Accepted", "03 Accepted, but Initially Declined"), na.rm = TRUE)/uptake_decision * 100, 1),
      # No Contact
      no_contact_12                   = sum(ldp %in% c("fail - PDS started more than 12 months after diagnosis", "fail - PDS not started and more than 12 months since diagnosis"), na.rm = TRUE),
      perc_no_contact_12              = round(no_contact_12/referrals * 100, 1),
      termination_before_contact      = sum(ldp == "fail - PDS terminated before first contact", na.rm = TRUE),
      perc_termination_before_contact = round(termination_before_contact/referrals * 100, 1),
      .groups = "drop") %>%
    # Add expected diagnoses data
    left_join(exp_df, by = c("geog", "fy")) %>%
    mutate(exp_rate = round(referrals / diagnoses * 100, 1)) 
  
  return(measures_df)
}



