################################################################################
# Name of file - setup_directories.R
# Original Authors - Jennifer Thom
# Original Date - November 2023
# Update - August 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Functions to set up working directory and declare file paths for
#               use in MI and Publication reports.
################################################################################

## Generic setup ##-------------------------------------------------------------

source(here::here("functions/general_setup.R"))


### MI directory setup ###------------------------------------------------------

#' Set up Management Information Directory
#' 
#' @description the function will use the fs package to return the file path to
#' the management-report directory. 
#'
#' @return the path to the management report folder.
#' 
#'
get_mi_dir <- function() {
  mi_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "management-report")
  
  return(mi_dir)
}


#' Set up the Management Information year directory
#'
#' @description The function will use the fs package to return the file path to the
#' management report year directory
#'
#' @param folder supply the data or output folder 
#'
#' @return the path to the management report year specific folder. 
#' 
#'
get_mi_year_dir <- function(folder = c("data", "output")) {
  year <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}")
  qtr <- stringr::str_glue("Q{qt}")
  
  year_dir <- fs::path(get_mi_dir(), {{ folder }}, year, qtr)
  
  if (!fs::dir_exists(year_dir)) {
    fs::dir_create(year_dir)
    cli::cli_alert_info(
      "{.path {year_dir}} did not exist, it has now been created."
    )
  }
  
  return(year_dir)
}


#' Get the path to the MI data outputs
#'
#' @description The function will return the file path to the data files needed
#' to create the MI report. 
#'
#' @param type Supply the type of data output
#' @param ext Supply the file extension
#'
#' @return the file path to the data files needed to create the MI report.
#' 
get_mi_data_path <- function(type = c(
  "error_data",
  "dupe_data",
  "clean_data",
  "ldp_data",
  "final_data"
),
ext = c("rds", "csv")) {
  file_name <- dplyr::case_match(
    type,
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes"),
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp"),
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data")
  )
  
  mi_data_path <- get_file_path(
    directory = get_mi_year_dir("data"),
    file_name = file_name,
    ext = ext
  )
  
  return(mi_data_path)
}


#' Get the path to the management report final output.
#' 
#' @description The function will return the path to the final management report 
#' html document for distribution
#'
#' @return the file path to the final mi report output in html format.
#' 
#' 
get_mi_output_path <- function() {
  file_name <- stringr::str_glue("{end_date}_management-report.html")
  
  mi_output_path <- get_file_path(
    directory = get_mi_year_dir("output"),
    file_name = file_name
  )
  
  return(mi_output_path)
}


### Publication directory setup ###---------------------------------------------

#' Set up the Publication Directory
#' 
#' @description the function will use the fs package to return the file path to
#' the annual publication directory
#'
#' @return the path to the publication folder
#' 
#'
get_pub_dir <- function() {
  pub_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "publication")
  
  return(pub_dir)
}


#' Set up the publication date directory
#' 
#' @description The function will use the fs package to return the file path to the
#' publication date directory
#'
#' @param folder supply the data or output folder 
#'
#' @return the path to the annual publication date folder
#'
#' 
get_pub_date_dir <- function(folder = c("data", "output")) {
  pub_date_dir <- fs::path("/","conf","dementia","A&I","Outputs","publication", 
                           {folder}, {pub_date})
  
  if (!fs::dir_exists(pub_date_dir)) {
    fs::dir_create(pub_date_dir)
    cli::cli_alert_info(
      "{.path {pub_date_dir}} did not exist, it has now been created."
    )
  }
  
  return(pub_date_dir)
}


#' Return the file path to the finalised publication data 
#' 
#' @description The publication data file is use to produce the annual publication
#'
#' @return the path to the finalised publication data
#'
#'
get_pub_data_path <- function() {
  file_name <- stringr::str_glue("{pub_date}_pub-data")
  
  pub_data_path <- get_file_path(
    directory = get_pub_date_dir("data"),
    file_name = file_name,
    ext = "rds"
  )
  
  return(pub_data_path)
}


#' Get the path to the final annual publication outputs
#' 
#' @description The output files are used for the publication: publication summary, 
#' publication report, excel tables and discovery data
#'
#' @param output_name Supply the output name
#'
#' @return the path to the output produced for the annual publication
#'
#' 
get_pub_output_path <- function(output_name = c("pub_summary", "pub_report", "excel_tables", "discovery_data")) {
  file_name <- dplyr::case_match(
    output_name,
    "pub_summary" ~ stringr::str_glue("{pub_date}_dementia-pds_summary.docx"),
    "pub_report" ~ stringr::str_glue("{pub_date}_dementia-pds_report.docx"),
    "excel_tables" ~ stringr::str_glue("{pub_date}_dementia-pds_excel-tables.xlsx"),
    "discovery_data" ~ stringr::str_glue("{pub_date}_ldp-data.csv")
  )
  
  pub_output_path <- get_file_path(
    directory = get_pub_date_dir("output"),
    file_name = file_name
  )
  
  return(pub_output_path)
}


#' Get the path to the final annual publication figures
#' 
#' @description These png files are used throughout the annual publication report
#' and summary document
#'
#' @param type The type of chart to return
#'
#' @return the path to the figures used for the annual publication
#'
#' 
get_pub_figures_path <- function(type = c("c1",
                                          "c2",
                                          "c3",
                                          "c4",
                                          "c5",
                                          "c6",
                                          "c7",
                                          "summary")) {
  
  file_name <- dplyr::case_match(
    type,
    "c1" ~ stringr::str_glue("{pub_date}_incidence-hb.png"),
    "c2" ~ stringr::str_glue("{pub_date}_12-months-hb.png"),
    "c3" ~ stringr::str_glue("{pub_date}_12-months-ijb.png"),
    "c4" ~ stringr::str_glue("{pub_date}_age-dist.png"),
    "c5" ~ stringr::str_glue("{pub_date}_12-months-age.png"),
    "c6" ~ stringr::str_glue("{pub_date}_simd-dist.png"),
    "c7" ~ stringr::str_glue("{pub_date}_12-months-simd.png"),
    "summary" ~ stringr::str_glue("{pub_date}_summary-chart.png")
  )
  
  pub_output_path <- get_file_path(
    directory = fs::path(get_pub_date_dir("output"), "figures"),
    file_name = file_name
  )
  
  return(pub_output_path)
}


### Static file paths ###-------------------------------------------------------

#' Set up the national directory folder
#'
#' @description The national folder is managed by the Data Management team who
#' produce the collated file for analysis
#'
#' @return the path to the national directory
#' 
#'
get_national_dir <- function(){
  
  national_dir <- fs::path("/", "conf", "dementia", "03-Outputs", "National")
  
  return(national_dir)
}


#' Get the path to the national collated file
#' 
#' @description the national collated file is produced by the data management team
#' for our analysis
#'
#' @return the path to the national collated file
#' 
#'
get_national_data_path <- function() {
  
  file_name <- stringr::str_glue("{fy}-Q{qt}_national.csv")
  
  national_data_path <- get_file_path(
    directory = get_national_dir(),
    file_name = file_name
  )
  
  return(national_data_path)
}


#' Get the path to the Aberdeen City lookup
#' 
#' @description Due to data quality issues (see SOP for guidance), a lookup was 
#' produced for using Aberdeen City figures in the annual publication
#'
#' @return the path to the aberdeen city lookup
#'
#' 
get_ac_lookup_path <- function() {
  
  file_name <- stringr::str_glue("aberdeen_city_lookup.xlsx")
  
  ac_lookup_path <- get_file_path(
    directory = fs::path(get_pub_dir(), "lookups"),
    file_name = file_name
  )
  
  return(ac_lookup_path)
}

# Path to 'Finalised data' files
# When a year becomes final this then becomes a static file
#' Get the path to the finalised data files 
#' 
#' @description When the annual publication produces a final file, this will be 
#' stored in this directory
#'
#' @return the path to the final data files 
#' 
#'
get_final_data_dir <- function() {
  
  final_data_dir <- fs::path(get_mi_dir(), "data", "final")
  
  return(final_data_dir)
}


#' Get the path to the reference-files directory
#'
#' @description The directory for storing the reference files: expected diagnoses 
#' and excel template.
#'
#' @return the path to the reference files directory
#'
#' 
get_ref_files_dir <- function(){
  ref_files_dir <- fs::path("/", "conf", "dementia", "A&I", "Outputs", "reference-files")
  return(ref_files_dir)
}


#' Get the path to the expected diagnoses file
#' 
#' @description the expected diagnoses file is needed to populate the management
#' report.
#'
#' @return The path to the expected diagnoses file
#'
#' 
get_exp_diagnoses_path <- function() {
  
  file_name <- stringr::str_glue("expected-diagnoses.csv")
  
  exp_diagnoses_path <- get_file_path(
    directory = get_ref_files_dir(),
    file_name = file_name
  )
  
  return(exp_diagnoses_path)
}


# excel template file
#' Get the path to the excel template for the annual publication
#' 
#' @description The excel template is needed for producing the figures for the 
#' annual publication.
#'
#' @return the path to the excel template
#'
#' 
get_excel_template_path <- function() {
  
  file_name <- stringr::str_glue("excel-template.xlsx")
  
  excel_template_file_path <- get_file_path(
    directory = get_ref_files_dir(),
    file_name = file_name
  )
  
  return(excel_template_file_path)
}


## End of Script ##
