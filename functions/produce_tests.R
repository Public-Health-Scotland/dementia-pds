#' Clean geography names
#' 
#' @description This function will remove the codes from Health Board and IJB
#' names. 
#' 
#' @param data `final_data` produced by the mi report containing `Health_Board` 
#' and `IJB` variables 
#' 
#' @return clean variable names 
#' @export
#'
clean_geog_codes <- function(data){
  
  data <- data %>% 
    mutate(health_board = str_sub(health_board, 3,-1),
           ijb = if_else(is.na(ijb),
                         "Unknown",
                         str_sub(ijb, 11,-1)))
  
  return(data)
}


#' Calculate cross year measures
#' 
#' @description This function will calculate the number of referrals across all
#' years available. This produces high level tests to show the trends.
#'
#' @param data `final_data` produced by the mi report. 
#' @param var variables within the `final_data`containing either `health_board` or 
#' `IJB`.
#' 
#' @return a dataframe with cross year tests
#' @export
#'
cross_year_measures <- function(data, var = c(health_board, ijb)){
  
  geog_data <- data %>% 
    # group by 
    group_by( {{ var }}, fy) %>%
    # summarise to get totals
    summarise(referrals = sum(referrals), .groups = "drop") %>% 
    # pivot to create table 
    pivot_wider(id_cols = {{ var }},
                names_from = fy, 
                values_from = referrals) %>%  
    rename(measure = {{ var }})
  
  scot_data <- latest_data %>% 
    # create Scotland rows
    mutate(measure = "Scotland") %>% 
    # group by 
    group_by(measure, fy) %>%
    # summarise to get totals
    summarise(referrals = sum(referrals), .groups = "drop") %>% 
    # pivot to create table 
    pivot_wider(names_from = fy, 
                values_from = referrals)
  
  
  summary <- bind_rows(geog_data, scot_data)
  
  return(summary)
}


#' Calculate measures 
#' @description This function will pass the data and variables needed and create 
#' a summary breakdown. 
#'
#' @param data `final_data` created by the mi report
#' @param var variables containing either `health_board` or `IJB`
#'
#' @return a dataframe with the total number of referrals at `Scotland`, 
#' `health_board`and `ijb` levels.
#' 
#' @export
#'
calculate_measures <- function(data, var = c(health_board, ijb)) { 

  geog_data <- data %>%
    group_by({{ var }}, fy) %>%
    summarise(referrals = sum(referrals), .groups = "drop") %>% 
    arrange(fy, {{ var }}) %>% 
    # Rename for adding other measures
    dplyr::rename("measure" = {{ var }}) 

  
  scot_data <- data %>%
    mutate(measure = "Scotland") %>% 
    group_by(fy, measure) %>%
    summarise(referrals = sum(referrals), .groups = "drop") %>% 
    arrange(fy, measure) 
  
  summary <- bind_rows(geog_data, scot_data)
  
return(summary)

}


#' Produce test comparison
#' 
#'
#' @param old_data dataframe containing the `final_data` in the previous quarter
#' mi report
#' @param new_data dataframe containing the `final_data` in the latest mi report
#'
#' @return a dataframe with a comparison of new and old data
#' @export
#'
produce_test_comparison <- function(old_data, new_data) {
  
  dplyr::full_join(old_data,
                   new_data,
                   by = c("measure", "fy"),
                   suffix = c("_old", "_new")
  ) %>%
    dplyr::arrange(measure, fy) %>%
    dplyr::mutate(
      difference = round(.data$referrals_new - .data$referrals_old, digits = 2L),
      pct_change = scales::percent(.data$difference / .data$referrals_old),
      # issue = !dplyr::between(.data$difference / .data$referrals_old, -0.05, 0.05), 
      issue = case_when(.data$difference / .data$referrals_old < -0.05 ~ "LOWER", 
                        .data$difference / .data$referrals_old > 0.05 ~ "GREATER", 
                        .data$difference / .data$referrals_old > -0.05 | 
                        .data$difference / .data$referrals_old< 0.05 ~ "NONE"
      )
    )
        
}
