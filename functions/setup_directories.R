################################################################################.
# Name of file - setup_directories.R
# Original Authors - Jennifer Thom
# Original Date - November 2023
# Update - August 2024
# Updated By - Lucy Binsted
# Date - July 2026
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Functions to set up working directory and declare file paths for
#               use in MI and Publication reports.
################################################################################.

################################################################################.
# Load general file path functions ----
################################################################################.

source(here::here("functions/setup_general.R"))

################################################################################.
# Root file path ----
################################################################################.

#' Get the root directory
#'
#' @description
#' Returns the root directory used to store outputs and supporting files.
#' This can be changed to save outputs in your own folder for testing code.
#'
#' @return
#' An [fs::path()] object containing the path to the
#' `dementia/A&I/Outputs` directory.
#'
#' @export

get_root_dir <- function() {
  #root_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs")
  root_dir <- fs::path("/", "conf", "dementia", "A&I", "Analysts", "Lucy", "test")
  
  return(root_dir)
}


################################################################################.
# MI file path functions ----
################################################################################.

#' Get the management report root directory
#'
#' @description
#' Returns the root directory used to store management report data,
#' outputs and supporting files.
#'
#' @return
#' An [fs::path()] object containing the path to the
#' `management-report` directory.
#'
#' @family management report file paths
#' @export

get_mi_dir <- function() {
  mi_dir <- fs::path(get_root_dir(), "management-report")
  
  return(mi_dir)
}


#' Get a management report year directory
#'
#' @description
#' Constructs and validates the path to a management report directory for a
#' specific financial year and quarter.
#'
#' The directory structure follows:
#'
#' \preformatted{
#' management-report/
#'   <folder>/
#'     <financial year>/
#'       Q<quarter>/
#' }
#'
#' When `test_output = TRUE`, an additional `test` subdirectory is appended.
#'
#' @param folder Character string specifying which management report folder to
#' return. One of:
#' \describe{
#'   \item{`"data"`}{Directory containing intermediate data files.}
#'   \item{`"output"`}{Directory containing final report outputs.}
#'   \item{`"tests"`}{Directory containing test files.}
#' }
#' @param fy Financial year start as a four-digit character string, e.g.
#' `"2025"` for financial year 2025-26.
#' @param qt Quarter number. Must be one of `1`, `2`, `3` or `4`.
#' @param test_output Logical. If `TRUE`, return the path to the `test`
#' subdirectory.
#' @param check_mode Access mode passed to [check_dir_path()]. One of
#' `"read"`, `"write"` or `"exists"`.
#' @param create Logical. If `TRUE`, create the directory (and any missing
#' parent directories) if it does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the requested directory path.
#'
#' @family management report file paths
#' @export

get_mi_year_dir <- function(folder = c("data", "output", "tests"), 
                            fy,
                            qt,
                            test_output = FALSE, 
                            check_mode = "read",
                            create = FALSE) {
  
  # Validate arguments
  folder <- match.arg(folder)
  
  # Check fy format
  if (!is.character(fy) || length(fy) != 1L ||
      !grepl("^[0-9]{4}$", fy)) {
    cli::cli_abort(
      "{.arg fy} must be a four-digit year, e.g. {.val \"2024\"}."
    )
  }
  
  # Check qt format
  if (!is.numeric(qt) || length(qt) != 1L || !qt %in% 1:4) {
    cli::cli_abort(
      "{.arg qt} must be one of {.val 1}, {.val 2}, {.val 3} or {.val 4}."
    )
  }

  # Construct the directory path
  year <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}")
  qtr <- stringr::str_glue("Q{qt}")
  test <- if (isTRUE(test_output)) "test" else ""
  mi_year_dir <- fs::path(get_mi_dir(), folder, year, qtr, test)
  
  # Check the directory path
  mi_year_dir <- check_dir_path(
    directory = mi_year_dir, 
    check_mode = check_mode,
    create = create
  )
  
  return(mi_year_dir)
}


#' Get the path to a management report data file
#'
#' @description
#' Constructs and validates the path to a management report data file for a
#' specified financial year and quarter.
#'
#' @param type Character string specifying the required data file:
#' \describe{
#'   \item{`"clean_data"`}{Cleaned source data.}
#'   \item{`"comp_data"`}{Comparator data.}
#'   \item{`"dupe_data"`}{Duplicate records report.}
#'   \item{`"error_data"`}{Error summary data.}
#'   \item{`"final_data"`}{Final analysis dataset.}
#'   \item{`"ldp_data"`}{Individuals with LDP data.}
#'   \item{`"ldp_wait_data"`}{LDP waiting list data.}
#'   \item{`"query_data"`}{Query summary data.}
#'   \item{`"query_error_data"`}{Query error summary data.}
#'   \item{`"uptake_data"`}{Uptake analysis data.}
#'   \item{`"wait_data"`}{Waiting list data.}
#' }
#' @param ext File extension. One of `"rds"` or `"csv"`.
#' @param fy Financial year start as a four-digit character string.
#' @param qt Quarter number. Must be one of `1`, `2`, `3` or `4`.
#' @param test_output Logical. If `TRUE`, use the test directory.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create_dir Logical. If `TRUE`, create the required directory if it
#' does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the requested data file path.
#'
#' @family management report file paths
#' @export

get_mi_data_path <- function(type = c("clean_data",
                                      "comp_data",
                                      "dupe_data",
                                      "error_data",
                                      "final_data",
                                      "ldp_data",
                                      "ldp_wait_data",
                                      "query_error_data",
                                      "query_data",
                                      "uptake_data",
                                      "wait_data"
                                      ),
                             ext = c("rds", "csv"), 
                             fy,
                             qt,
                             test_output = FALSE, 
                             check_mode = "read",
                             create_dir = FALSE) {
  
  # Validate arguments
  type <- match.arg(type)
  ext <- match.arg(ext)
  
  # Get and validate the management report folder for a specific year
  mi_year_dir <- get_mi_year_dir(
    folder = "data", 
    fy = fy,
    qt = qt,
    test_output = test_output, 
    check_mode = check_mode,
    create = create_dir
  )
    
  # Get the file name
  file_name <- dplyr::case_match(
    type,
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data"),
    "comp_data" ~ stringr::str_glue("{fy}-{qt}_comp-data"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes"),
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary"),
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data.rds"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp"),
    "ldp_wait_data" ~ stringr::str_glue("{fy}-{qt}_ldp_wait-data"),
    "query_error_data" ~ stringr::str_glue("{fy}-{qt}_query-error-summary"),
    "query_data" ~ stringr::str_glue("{fy}-{qt}_query-summary"),
    "uptake_data" ~ stringr::str_glue("{fy}-{qt}_uptake-data"),
    "wait_data" ~ stringr::str_glue("{fy}-{qt}_wait-data"),
    )
  
  # Check the file path
  mi_data_path <- check_file_path(
    directory = mi_year_dir,
    file_name = file_name,
    ext = ext,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(mi_data_path)
}


#' Get the path to the final management report output
#'
#' @description
#' Constructs and validates the path to the final management report HTML file
#' for a specified financial year and quarter.
#'
#' The output file name is based on the quarter end date. For example:
#'
#' \preformatted{
#' 2026-03-31_management-report.html
#' }
#'
#' for financial year `"2025"` quarter `4`.
#'
#' @param fy Financial year start as a four-digit character string.
#' @param qt Quarter number. Must be one of `1`, `2`, `3` or `4`.
#' @param test_output Logical. If `TRUE`, return a path within the test output
#' directory.
#' @param check_mode Access mode required for the file. One of `"read"` or
#' `"write"`.
#' @param create_dir Logical. If `TRUE`, create the output directory if it does
#' not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the final HTML
#' management report.
#'
#' @family management report file paths
#' @export

get_mi_output_path <- function(fy,
                               qt,
                               test_output = FALSE,
                               check_mode = "read",
                               create_dir = FALSE) {
  
  # Get and validate the management report folder for a specific year
  mi_year_dir <- get_mi_year_dir(
    folder = "output", 
    fy = fy,
    qt = qt,
    test_output = test_output, 
    check_mode = check_mode,
    create = create_dir
  )
  
  # Construct the end date for the fy and qt provided
  year <- as.integer(fy) + (as.integer(qt) == 4L)
  month <- c(6, 9, 12, 3)[as.integer(qt)]
  
  end_date <- lubridate::ceiling_date(
    lubridate::ymd(sprintf("%04d-%02d-01", year, month)),
    "month") - lubridate::days(1)
  
  # Get the file name
  file_name <- stringr::str_glue("{end_date}_management-report.html")
  
  # Check the file path
  mi_output_path <- check_file_path(
    directory = mi_year_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(mi_output_path)
}


#' Get the processed population file paths
#'
#' @description
#' Constructs and validates the path to the processed population files used
#' within the management report process.
#'
#' @param simd Logical. If `TRUE`, returns the DataZone population with SIMD information.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the population
#' lookup file.
#'
#' @family management report file paths
#' @export

get_pop_lookup_path <- function(simd = FALSE,
                                check_mode = "read",
                                create_dir = FALSE) {
  
  # Construct the directory path
  lookup_dir <- fs::path(get_mi_dir(), "lookups")
  
  # Get the file name
  file_name <- ifelse(simd, "simd_pop_data.rds", "pop_data.rds")
  
  # Check the file path
  pop_lookup_path <- check_file_path(
    directory = lookup_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(pop_lookup_path)
}


#' Get the final data directory
#'
#' @description
#' Returns the directory used to store finalised management information
#' datasets.
#'
#' Once a reporting year is finalised, the corresponding files are stored
#' within this directory as static reference datasets.
#'
#' @return
#' An [fs::path()] object containing the path to the final data directory.
#'
#' @family management report file paths
#' @export

get_finalised_data_dir <- function() {
  finalised_data_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", "final")
  
  return(finalised_data_dir)
}


#' Get the path to a finalised management information dataset
#'
#' @description
#' Constructs and validates the path to a finalised management information
#' dataset for a specified financial year.
#'
#' Once a reporting year has been finalised, the corresponding dataset is
#' stored as a static reference file within the final data directory.
#'
#' Expected file names follow the pattern:
#'
#' \preformatted{
#' <financial year>_final-data.rds
#' }
#'
#' For example:
#'
#' \preformatted{
#' 2024-25_final-data.rds
#' }
#'
#' @param fy Financial year start as a four-digit character string, e.g.
#' `"2024"` for financial year 2024-25.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create_dir Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the finalised
#' management information dataset.
#'
#' @family management report file paths
#' @export

get_finalised_data_path <- function(fy,
                                    check_mode = "read",
                                    create_dir = FALSE) {
  
  # Check fy format
  if (!is.character(fy) || length(fy) != 1L ||
      !grepl("^[0-9]{4}$", fy)) {
    cli::cli_abort(
      "{.arg fy} must be a four-digit year, e.g. {.val \"2024\"}."
    )
  }
  
  # Construct the directory path
  finalised_data_dir <- get_finalised_data_dir()
  
  # Get the file name
  file_name <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}_final-data.rds")
  
  # Check the file path
  finalised_data_path <- check_file_path(
    directory = finalised_data_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(finalised_data_path)
}


#' Get the Aberdeen City LDP data directory
#'
#' @description
#' Returns the directory containing Aberdeen City Local Delivery Plan (LDP)
#' dementia post-diagnostic support datasets for financial years 2019-20 and
#' 2020-21.
#'
#' These files were created to support reporting for Aberdeen City and to
#' address known data quality issues affecting the management information
#' process. Refer to the relevant SOP for further details.
#'
#' @return
#' An [fs::path()] object containing the path to the Aberdeen City LDP
#' data directory.
#'
#' @family management report file paths
#' @export

get_ac_data_dir <- function() {
  ac_data_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", "Aberdeen City ldp files")
  
  return(ac_data_dir)
}


#' Get the path to an Aberdeen City LDP dataset
#'
#' @description
#' Constructs and validates the path to an Aberdeen City Local Delivery Plan
#' (LDP) dementia post-diagnostic support dataset for a specified financial
#' year.
#'
#' These files were created to support reporting for Aberdeen City and to
#' address known data quality issues affecting the management information
#' process. Refer to the relevant SOP for further details.
#'
#' Expected file names follow the pattern:
#'
#' \preformatted{
#' <financial year>_individuals-with-ldp_aberdeen-city.csv
#' }
#'
#' For example:
#'
#' \preformatted{
#' 2019-20_individuals-with-ldp_aberdeen-city.csv
#' }
#'
#' @param fy Financial year start as a four-digit character string, e.g.
#' `"2019"` for financial year 2019-20.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create_dir Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the Aberdeen City
#' LDP dataset.
#'
#' @family management report file paths
#' @export

get_ac_data_path <- function(fy,
                             ext = "rds",
                             check_mode = "read",
                             create_dir = FALSE) {
  
  # Check fy format
  if (!is.character(fy) || length(fy) != 1L ||
      !grepl("^[0-9]{4}$", fy)) {
    cli::cli_abort(
      "{.arg fy} must be a four-digit year, e.g. {.val \"2024\"}."
    )
  }
  
  # Construct the directory path
  ac_data_dir <- get_ac_data_dir()

  # Get the file name
  file_name <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}_individuals-with-ldp_aberdeen-city")
  
  # Check the file path
  ac_data_path <- check_file_path(
    directory = ac_data_dir,
    file_name = file_name,
    ext = ext,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(ac_data_path)
}


################################################################################.
# Publication file path functions ----
################################################################################.

#' Get the publication root directory
#'
#' @description
#' Returns the root directory used to store annual publication data,
#' outputs and supporting files.
#'
#' @return
#' An [fs::path()] object containing the path to the publication root
#' directory.
#'
#' @family publication file paths
#' @export

get_pub_dir <- function() {
  pub_dir <- fs::path(get_root_dir(), "publication")
  return(pub_dir)
}


#' Get a publication date directory
#'
#' @description
#' Constructs and validates the path to a publication directory for a
#' specified publication date.
#'
#' The directory structure follows:
#'
#' \preformatted{
#' publication/
#'   <folder>/
#'     <publication date>/
#' }
#'
#' When `test_output = TRUE`, an additional `test` subdirectory is appended.
#'
#' @param folder Character string specifying which publication folder to
#' return. One of:
#' \describe{
#'   \item{`"data"`}{Directory containing publication datasets.}
#'   \item{`"output"`}{Directory containing publication outputs.}
#' }
#' @param pub_date A Date object representing the publication date.
#' For example:
#' 
#' \preformatted{
#' lubridate::dmy("30062026")
#' }
#' @param test_output Logical. If `TRUE`, return the path to the `test`
#' subdirectory.
#' @param check_mode Access mode passed to [check_dir_path()]. One of
#' `"read"`, `"write"` or `"exists"`.
#' @param create Logical. If `TRUE`, create the directory (and any missing
#' parent directories) if it does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the requested directory path.
#'
#' @family publication file paths
#' @export

get_pub_date_dir <- function(folder = c("data", "output", "figures"), 
                             pub_date,
                             test_output = FALSE, 
                             check_mode = "read",
                             create = FALSE) {
  
  # Validate arguments
  folder <- match.arg(folder)
  
  # Check pub_date format
  if (!inherits(pub_date, "Date")) {
    cli::cli_abort(
      "{.arg pub_date} must be a Date object, e.g. {.code lubridate::dmy(\"30062026\")}."
    )
  }
  
  # Construct the directory path
  if (isTRUE(test_output) && folder == "figures"){
    cli::cli_alert_info("Test folder is not available for figures. Returning non-test folder.")
    sub_folder <- "figures"
  } else if (folder == "figures"){
    sub_folder <- "figures"
  } else if(isTRUE(test_output)){
    sub_folder <- "test"
  } else {
    sub_folder <- ""
  }
  folder <- if (folder == "figures") "output" else folder
  pub_date_dir <- fs::path(get_pub_dir(), folder, pub_date, sub_folder)
  
  # Check the directory path
  pub_date_dir <- check_dir_path(
    directory = pub_date_dir, 
    check_mode = check_mode,
    create = create
  )
  
  return(pub_date_dir)
}


#' Get the publication data file path
#'
#' @description
#' Constructs and validates the path to the final publication dataset used
#' to produce the annual publication.
#'
#' The expected file name follows the pattern:
#'
#' \preformatted{
#' <pub_date>_pub-data.rds
#' }
#'
#' @param pub_date A Date object representing the publication date.
#' For example:
#' 
#' \preformatted{
#' lubridate::dmy("30062026")
#' }
#' @param test_output Logical. If `TRUE`, use the test data directory.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create_dir Logical. If `TRUE`, create the required directory if it
#' does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the publication
#' dataset.
#'
#' @family publication file paths
#' @export

get_pub_data_path <- function(pub_date,
                              test_output = FALSE,
                              check_mode = "read",
                              create_dir = FALSE) {
  
  # Get and validate the publication folder for a specific date
  pub_date_dir <- get_pub_date_dir(
    folder = "data", 
    pub_date = pub_date,
    test_output = test_output, 
    check_mode = check_mode,
    create = create_dir
  )
  
  # Get the file name
  file_name <- stringr::str_glue("{pub_date}_pub-data.rds")
  
  # Check the file path
  pub_data_path <- check_file_path(
    directory = pub_date_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(pub_data_path)
}


#' Get the path to a publication output file
#'
#' @description
#' Constructs and validates the path to an output file produced as part of the
#' annual publication.
#'
#' @param output_name Character string specifying the required output file:
#' \describe{
#'   \item{`"pub_summary"`}{Publication summary document.}
#'   \item{`"pub_report"`}{Publication report document.}
#'   \item{`"excel_tables"`}{Publication Excel tables.}
#'   \item{`"discovery_data"`}{Discovery dataset.}
#' }
#' @param pub_date A Date object representing the publication date.
#' For example:
#'
#' \preformatted{
#' lubridate::dmy("30062026")
#' }
#' @param test_output Logical. If `TRUE`, use the test output directory.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create_dir Logical. If `TRUE`, create the required directory if it
#' does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the requested publication output
#' file path.
#'
#' @family publication file paths
#' @export

get_pub_output_path <- function(output_name = c("pub_summary", 
                                                "pub_report", 
                                                "excel_tables", 
                                                "discovery_data"),
                                pub_date,
                                test_output = FALSE,
                                check_mode = "read",
                                create_dir = FALSE) {
  
  # Validate arguments
  output_name <- match.arg(output_name)

  # Get and validate the publication folder for a specific date
  pub_date_dir <- get_pub_date_dir(
    folder = "output", 
    pub_date = pub_date,
    test_output = test_output, 
    check_mode = check_mode,
    create = create_dir
  )
  
  # Get the file name
  file_name <- dplyr::case_match(
    output_name,
    "pub_summary" ~ stringr::str_glue("{pub_date}_dementia-pds_summary.docx"),
    "pub_report" ~ stringr::str_glue("{pub_date}_dementia-pds_report.docx"),
    "excel_tables" ~ stringr::str_glue("{pub_date}_dementia-pds_excel-tables.xlsx"),
    "discovery_data" ~ stringr::str_glue("{pub_date}_ldp-data.csv")
  )
  
  # Check the file path
  pub_output_path <- check_file_path(
    directory = pub_date_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(pub_output_path)
}


#' @param type Character string specifying the required figure:
#' \describe{
#'   \item{`"c1"`}{Incidence by Health Board.}
#'   \item{`"c2"`}{12 month trend by Health Board.}
#'   ...
#'   \item{`"c14"`}{Waiting times trend chart.}
#'   \item{`"summary"`}{Summary infographic chart.}
#'   \item{`"twitter"`}{Social media summary chart.}
#' }
#'
#' @param pub_date A Date object representing the publication date.
#'
#' @param test_output Logical. If `TRUE`, use the test figures directory.
#'
#' @param check_mode Access mode required for the file. One of
#' `"read"` or `"write"`.
#'
#' @param create_dir Logical. If `TRUE`, create the required directory
#' if it does not already exist.
#'
#' @return
#' A validated [fs::path()] object containing the requested figure file path.
#' 
#' @family publication file paths
#' @export

get_pub_figures_path <- function(type = c("c1",
                                          "c2",
                                          "c3",
                                          "c4",
                                          "c5",
                                          "c6",
                                          "c7",
                                          "c8",
                                          "c9",
                                          "c10",
                                          "c11",
                                          "c12",
                                          "c13",
                                          "c14",
                                          "summary"), 
                                 pub_date,
                                 test_output = FALSE,
                                 check_mode = "read",
                                 create_dir = FALSE) {
  # Validate arguments
  type <- match.arg(type)
  
  # Get and validate the publication folder for a specific date
  pub_date_dir <- get_pub_date_dir(
    folder = "figures", 
    pub_date = pub_date,
    test_output = test_output, 
    check_mode = check_mode,
    create = create_dir
  )
  
  # Get the file name
  file_name <- dplyr::case_match(
    type,
    "c1" ~ stringr::str_glue("{pub_date}_incidence-hb.png"),
    "c2" ~ stringr::str_glue("{pub_date}_12-months-hb.png"),
    "c3" ~ stringr::str_glue("{pub_date}_12-months-ijb.png"),
    "c4" ~ stringr::str_glue("{pub_date}_age-dist.png"),
    "c5" ~ stringr::str_glue("{pub_date}_12-months-age.png"),
    "c6" ~ stringr::str_glue("{pub_date}_simd-dist.png"),
    "c7" ~ stringr::str_glue("{pub_date}_12-months-simd.png"),
    "c8" ~ stringr::str_glue("{pub_date}_total_referrals_trend.png"),
    "c9" ~ stringr::str_glue("{pub_date}_rate_trend.png"),
    "c10" ~ stringr::str_glue("{pub_date}_sex-dist.png"),
    "c11" ~ stringr::str_glue("{pub_date}_12-months-sex.png"),
    "c12" ~ stringr::str_glue("{pub_date}_wait-times-hb.png"),
    "c13" ~ stringr::str_glue("{pub_date}_wait-times-ijb.png"),
    "c14" ~ stringr::str_glue("{pub_date}_wait-times-trend.png"),
    "summary" ~ stringr::str_glue("{pub_date}_summary-chart.png")
  )
  
  # Check the file path
  pub_fig_path <- check_file_path(
    directory = pub_date_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(pub_fig_path)
}


#' Get the Aberdeen City lookup file path
#'
#' @description
#' Constructs and validates the path to the Aberdeen City lookup file used
#' within the annual publication process.
#'
#' The lookup was created to address known data quality issues affecting
#' Aberdeen City reporting. Refer to the relevant SOP for further details.
#'
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the Aberdeen City
#' lookup file.
#'
#' @family publication file paths
#' @export

get_ac_lookup_path <- function(check_mode = "read",
                               create_dir = FALSE) {
  
  # Construct the directory path
  lookup_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "publication", "lookups")
  
  # Get the file name
  file_name <- "aberdeen_city_lookup.xlsx"
  
  # Check the file path
  ac_lookup_path <- check_file_path(
    directory = lookup_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(ac_lookup_path)
}


#' Get the Excel template file path
#'
#' @description
#' Constructs and validates the path to the Excel template used to produce
#' annual publication figures and supporting outputs.
#'
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the Excel template
#' file.
#'
#' @family publication file paths
#' @export

get_excel_template_path <- function(check_mode = "read",
                                    create_dir = FALSE) {
  
  # Construct the directory path
  template_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "publication", "templates")
  
  # Get the file name
  file_name <- stringr::str_glue("excel-template.xlsx")
  
  # Check the file path
  excel_template_file_path <- check_file_path(
    directory = template_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(excel_template_file_path)
}


################################################################################.
# Data Management file path functions ----
################################################################################.

#' Get the national data directory
#'
#' @description
#' Returns the root directory containing national collated data files produced
#' by the Data Management team.
#'
#' These files are used as the source data for national dementia analysis.
#'
#' @return
#' An [fs::path()] object containing the path to the national data directory.
#'
#' @family data management file paths
#' @export

get_national_dir <- function(){
  national_dir <- fs::path("/", "conf", "dementia", "03-Outputs", "National")
  
  return(national_dir)
}


#' Get the path to a national collated data file
#'
#' @description
#' Constructs and validates the path to a national collated data file for a
#' specified financial year and quarter.
#'
#' The function first searches the National directory. If the file is not found,
#' it then searches within the corresponding financial year subdirectory.
#'
#' Expected file names follow the pattern:
#'
#' \preformatted{
#' <fy>-Q<qt>_national.csv
#' }
#'
#' For example:
#'
#' \preformatted{
#' 2024-Q4_national.csv
#' }
#'
#' @param fy Financial year start as a four-digit character string, e.g.
#' `"2024"` for financial year 2024-25.
#' @param qt Quarter number. Must be one of `1`, `2`, `3` or `4`.
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#' 
#' @return
#' A validated [fs::path()] object containing the path to the national
#' collated data file.
#'
#' @family data management file paths
#' @export

get_national_data_path <- function(fy, 
                                   qt,
                                   check_mode = "read") {
  
  # Check fy format
  if (!is.character(fy) || length(fy) != 1L ||
      !grepl("^[0-9]{4}$", fy)) {
    cli::cli_abort(
      "{.arg fy} must be a four-digit year, e.g. {.val \"2024\"}."
    )
  }
  
  # Check qt format
  if (!is.numeric(qt) || length(qt) != 1L || !qt %in% 1:4) {
    cli::cli_abort(
      "{.arg qt} must be one of {.val 1}, {.val 2}, {.val 3} or {.val 4}."
    )
  }
  
  # Construct the directory paths
  national_dir <- get_national_dir()
  national_year_dir <- fs::path(national_dir, stringr::str_glue("{fy}-{as.integer(fy)+1}"))
  
  # Get the file name
  file_name <- stringr::str_glue("{fy}-Q{qt}_national.csv")
  
  # Check the file path using the top level folder
  national_data_path <- tryCatch(
    check_file_path(
      directory = national_dir,
      file_name = file_name,
      check_mode = check_mode,
      create_dir = FALSE
    ),
    # If the file does not exist, check the year sub-folder 
    error = function(e) {
      if (!grepl("does not exist in", conditionMessage(e))) {
        stop(e)
      }
      check_file_path(
        directory = national_year_dir,
        file_name = file_name,
        check_mode = check_mode,
        create_dir = FALSE
      )
    }
  )
  
  return(national_data_path)
}


################################################################################.
# Reference file path functions ----
################################################################################.

#' Get the reference files directory
#'
#' @description
#' Returns the directory containing reference files used throughout the
#' reporting and publication processes.
#'
#' Examples include expected diagnosis datasets and publication templates.
#'
#' @return
#' An [fs::path()] object containing the path to the reference files directory.
#'
#' @family reference file paths
#' @export
 
get_ref_files_dir <- function(){
  ref_files_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "reference-files")
  
  return(ref_files_dir)
}


#' Get the expected diagnoses file path
#'
#' @description
#' Constructs and validates the path to the expected diagnoses dataset used
#' to populate management information reports.
#'
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the expected
#' diagnoses file.
#'
#' @family reference file paths
#' @export

get_exp_diagnoses_path <- function(check_mode = "read",
                                   create_dir = FALSE) {
  # Construct the directory path
  ref_files_dir <- get_ref_files_dir()
  
  # Get the file name
  file_name <- "expected-diagnoses.csv"
  
  # Check the file path
  exp_diagnoses_path <- check_file_path(
    directory = ref_files_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(exp_diagnoses_path)
}


#' Get the European Standard Population file path
#'
#' @description
#' Constructs and validates the path to the European Standard Population
#' lookup dataset used when calculating age-standardised rates.
#'
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param create Logical. If `TRUE`, create the directory if it does not
#' already exist.
#'
#' @return
#' A validated [fs::path()] object containing the path to the European
#' Standard Population lookup file.
#'
#' @family reference file paths
#' @export

get_esp_path <- function(check_mode = "read",
                         create_dir = FALSE) {
  
  # Construct the directory path
  ref_files_dir <- get_ref_files_dir()
  
  # Get the file name
  file_name <- "european_standard_population_by_sex.csv"

  # Check the file path
  esp_path <- check_file_path(
    directory = ref_files_dir,
    file_name = file_name,
    check_mode = check_mode,
    create_dir = create_dir
  )
  
  return(esp_path)
}


################################################################################.
# Linkage file path functions ----
################################################################################.

#' Get the linkage files directory
#'
#' @description
#' Returns the root directory containing centrally managed linkage and
#' lookup files.
#'
#' @return
#' An [fs::path()] object containing the path to the linkage files
#' directory.
#'
#' @family lookup file paths
#' @export

get_linkage_files_dir <- function(){
  linkage_files_dir <-  fs::path("/", "conf", "linkage", "output", "lookups", "Unicode")
  
  return(linkage_files_dir)
}


#' Get the SIMD lookup file path
#'
#' @description
#' Locates and validates the centrally held Scottish Index of Multiple
#' Deprivation (SIMD) lookup file.
#'
#' When multiple matching files are found, a single file is selected
#' according to `selection_method`.
#'
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param selection_method Method used to select a file when multiple
#' matches are found. One of `"modification_date"` or `"file_name"`.
#' @param recurse Logical. Should nested directories also be searched?
#'
#' @return
#' A validated [fs::path()] object containing the selected SIMD lookup file.
#'
#' @family lookup file paths
#' @export
#'
#' @examples
#' \dontrun{
#' get_simd_path()
#' }

get_simd_path <- function(check_mode = "read",
                          selection_method = "modification_date",
                          recurse = FALSE) {
  
  # Construct the directory path
  simd_dir <- fs::path(get_linkage_files_dir(), "Deprivation")
  
  # Get the file name regular expression
  file_name_regexp <- "postcode_\\d\\d\\d\\d_\\d_simd\\d\\d\\d\\d.*?\\.rds$"
  
  # Check the file path
  simd_path <- check_file_path(
    directory = simd_dir,
    check_mode = check_mode,
    create_dir = FALSE,
    file_name_regexp = file_name_regexp,
    selection_method = selection_method,
    recurse = recurse
  )
  
  return(simd_path)
}


#' Get a population estimates file path
#'
#' @description
#' Locates and validates a population estimates dataset for a specified
#' geography type.
#'
#' The function searches for files matching the naming convention:
#'
#' \preformatted{
#' <type><year>_pop_est_<start_year>_<end_year>.rds
#' }
#'
#' For example:
#'
#' \preformatted{
#' HSCP2019_pop_est_1981_2023.rds
#' }
#'
#' @param type Geography type. One of:
#' \describe{
#'   \item{`"HB"`}{Health Board population estimates.}
#'   \item{`"HSCP"`}{Health and Social Care Partnership estimates.}
#'   \item{`"DataZone"`}{Data Zone population estimates.}
#' }
#' @param check_mode Access mode required for the file. Passed to
#' [check_file_path()]. One of `"read"` or `"write"`.
#' @param selection_method Method used to select a file when multiple
#' matches are found. One of `"modification_date"` or `"file_name"`.
#' @param recurse Logical. Should nested directories also be searched?
#'
#' @return
#' A validated [fs::path()] object containing the selected population
#' estimates file.
#'
#' @family lookup file paths
#' @export
#'
#' @examples
#' \dontrun{
#' get_pop_path("HSCP")
#' }

get_pop_path <- function(type = c("HB", "HSCP", "DataZone"),
                         check_mode = "read",
                         selection_method = "modification_date",
                         recurse = FALSE){
  
  # Validate arguments
  type <- match.arg(type)
  
  # Construct the directory path
  pop_dir <- fs::path(get_linkage_files_dir(), "Populations", "Estimates")
  
  # Get the file name regular expression
  file_name_regexp <- stringr::str_glue(
    "{type}[0-9]{{4}}_pop_est_[0-9]{{4}}_[0-9]{{4}}\\.rds$"
  )
  
  # Check the file path
  pop_path <- check_file_path(
    directory = pop_dir,
    check_mode = check_mode,
    create_dir = FALSE,
    file_name_regexp = file_name_regexp,
    selection_method = selection_method,
    recurse = recurse
  )
  
  return(pop_path)
}

################################ End of Script #################################.