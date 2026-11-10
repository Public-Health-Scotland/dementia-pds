#' Write tests to xlsx
#'
#' @description This function will pass the tests data and write to an xlsx workbook
#'
#' @param data `test_data` produced by the function `calculate_measures`
#'
#' @return a workbook containing tests for the MI report data.
#' @export

write_tests_xlsx <- function(comparison_data, 
                             sheet_name) {
  
  year <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}")
  qtr <- stringr::str_glue("Q{qt}")  
  
  workbook_name <- stringr::str_glue("{year}_{qtr}_mi_report_tests")
  
  tests_workbook_dir <- get_mi_year_dir(
    folder = "tests",
    fy = fy,
    qt = qt,
    test_output = test_output, 
    check_mode = "write",
    create = TRUE
  )
  
  tests_workbook_path <- fs::path(
    tests_workbook_dir, 
    workbook_name, 
    ext = "xlsx"
  )
  
  if (fs::file_exists(tests_workbook_path)) {
    # Load the data from the existing workbook
    wb <- openxlsx::loadWorkbook(tests_workbook_path)
  } else {
    # Create a blank workbook object
    wb <- openxlsx::createWorkbook()
  }
  
  # Add a new sheet with date 
  date_today <- format(Sys.Date(), "%d_%b")
  date_today <- stringr::str_to_lower(date_today)
  
  sheet_name_dated <- stringr::str_glue("{sheet_name}_{date_today}")
  
  # If there has already been a sheet created today, append the time
  if (sheet_name_dated %in% names(wb)) {
    sheet_name_dated <- paste0(sheet_name_dated, format(Sys.time(), "_%H%M"))
  }
  
  # Add new worksheet
  openxlsx::addWorksheet(wb, sheet_name_dated)
  
  # Write test comparison output to the new sheet
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
  openxlsx::saveWorkbook(
    wb,
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


#' Produce test comparison
#'
#' @param old_data dataframe containing the `final_data` in the previous quarter
#' mi report
#' @param new_data dataframe containing the `final_data` in the latest mi report
#'
#' @return a dataframe with a comparison of new and old data
#' @export
produce_test_comparison <- function(old_data, new_data) {
  dplyr::full_join(old_data,
    new_data,
    by = c("IAA/HB", "fy"),
    suffix = c("_old", "_new")
  ) %>%
    #dplyr::arrange(measure, fy) %>%
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
