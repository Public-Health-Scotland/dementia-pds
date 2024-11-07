#' Write tests to xlsx
#'
#' @description This function will pass the tests data and write to an xlsx workbook
#'
#' @param data `test_data` produced by the function `calculate_measures`
#'
#' @return a workbook containing tests for the MI report data.
#' @export
#'
write_tests_xlsx <- function(comparison_data, 
                             sheet_name) {
  
  year <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}")
  qtr <- stringr::str_glue("Q{qt}")  
  
  workbook_name <- stringr::str_glue("{year}_{qtr}_mi_report_tests")
  
  tests_workbook_path <- fs::path(get_mi_year_dir("tests"), 
                                  workbook_name, 
                                  ext = "xlsx")
  
  if (fs::file_exists(tests_workbook_path)) {
    # Load the data from the existing workbook
    wb <- openxlsx::loadWorkbook(tests_workbook_path)
  } else {
    # Create a blank workbook object
    wb <- openxlsx::createWorkbook()
  }
  
  # add a new sheet with date 
  date_today <- format(Sys.Date(), "%d_%b")
  date_today <- stringr::str_to_lower(date_today)
  
  sheet_name_dated <- stringr::str_glue("{sheet_name}_{date_today}")
  
  # If there has already been a sheet created today, append the time
  if (sheet_name_dated %in% names(wb)) {
    sheet_name_dated <- paste0(sheet_name_dated, format(Sys.time(), "_%H%M"))
  }
  
  # Add new worksheet
  openxlsx::addWorksheet(wb, sheet_name_dated)
  
  # write test comparison output to the new sheet
  # Style it as a Data table for nice formatting
  openxlsx::writeDataTable(
    wb = wb,
    sheet = sheet_name_dated,
    x = comparison_data,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE
  )
  
  # Formatting -----------------------------------------------------------------
  
  # Get the column numbers
  pct_change_col <- which(
    names(comparison_data) == "pct_change"
  )
  issue_col <- which(
    names(comparison_data) == "issue"
  )
  numeric_cols <- which(
    names(comparison_data) %in% c("referrals_old", "referrals_new", "difference")
  )
  
  # Format the pct_change column as a percentage
  openxlsx::addStyle(
    wb = wb,
    sheet = sheet_name_dated,
    style = openxlsx::createStyle(numFmt = "0.0%"),
    cols = pct_change_col,
    rows = 2L:(nrow(comparison_data) + 1L),
    gridExpand = TRUE
  )
  
  # Format the numeric columns with commas
  openxlsx::addStyle(
    wb = wb,
    sheet = sheet_name_dated,
    style = openxlsx::createStyle(numFmt = "#,##0"),
    cols = numeric_cols,
    rows = 2L:(nrow(comparison_data) + 1L),
    gridExpand = TRUE
  )
  
  # Set the column widths - wider for the first (measure)
  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name_dated,
    cols = 1L,
    widths = 40L
  )
  
  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name_dated,
    cols = 2L:ncol(comparison_data),
    widths = 15L
  )
  

  # Write workbook to disk -----------------------------------------------------
  
  # Reorder the sheets alphabetically
  sheet_names <- wb$sheet_names
  names(sheet_names) <- wb$sheetOrder
  
  openxlsx::worksheetOrder(wb) <- names(sort(sheet_names))
  
  # Write the data to the workbook on disk
  openxlsx::saveWorkbook(wb,
                         tests_workbook_path,
                         overwrite = TRUE
  )
  
  if (fs::file_info(tests_workbook_path)$user == Sys.getenv("USER")) {
    # Set the correct permissions
    fs::file_chmod(path = tests_workbook_path, mode = "770")
    fs::file_chown(path = tests_workbook_path, group_id = 3182)
    
  }
  
  cli::cli_alert_success(
    "The tests for {workbook_name} were written to {.file {fs::path_file(tests_workbook_path)}}"
  )
  
}


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
clean_geog_codes <- function(data) {
  data <- data %>%
    mutate(
      health_board = str_sub(health_board, 3, -1),
      ijb = if_else(is.na(ijb),
        "Unknown",
        str_sub(ijb, 11, -1)
      )
    )

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
cross_year_measures <- function(data, var = c(health_board, ijb)) {
  geog_data <- data %>%
    # group by
    group_by({{ var }}, fy) %>%
    # summarise to get totals
    summarise(referrals = sum(referrals), .groups = "drop") %>%
    # pivot to create table
    pivot_wider(
      id_cols = {{ var }},
      names_from = fy,
      values_from = referrals
    ) %>%
    rename(measure = {{ var }})

  scot_data <- latest_data %>%
    # create Scotland rows
    mutate(measure = "Scotland") %>%
    # group by
    group_by(measure, fy) %>%
    # summarise to get totals
    summarise(referrals = sum(referrals), .groups = "drop") %>%
    # pivot to create table
    pivot_wider(
      names_from = fy,
      values_from = referrals
    )


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
      issue = case_when(
        .data$difference / .data$referrals_old < -0.05 ~ "LOWER",
        .data$difference / .data$referrals_old > 0.05 ~ "GREATER",
        .data$difference / .data$referrals_old > -0.05 |
          .data$difference / .data$referrals_old < 0.05 ~ "NONE"
      )
    )
}
