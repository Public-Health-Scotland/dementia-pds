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

# Use general file path functions
source(here::here("functions/setup_general.R"))

# Use write file function for writing files to disk and setting correct permissions
source(here::here("functions/write_file.R"))

# Use render_check function for rendering rmarkdown files
source(here::here("functions/render_check.R"))

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
get_mi_year_dir <- function(folder = c("data", "output", "tests"), 
                            test_output = FALSE, 
                            previous_data = FALSE,
                            previous_year_to_qt = FALSE) {
  
  if (previous_data){
    year <- stringr::str_glue("{previous_fy}-{substr(as.numeric(previous_fy)+1, 3, 4)}")
    qtr <- stringr::str_glue("Q{previous_qt}")
    test <- "test"

  } else if(previous_year_to_qt){
      year <- stringr::str_glue("{as.numeric(fy)-1}-{substr(as.numeric(fy), 3, 4)}")
      qtr <- stringr::str_glue("Q{qt}")
      test <- "test"
    
  } else {
  year <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}")
  qtr <- stringr::str_glue("Q{qt}")
  test <- "test"
  }

  if ((test_output)){
    year_dir <- fs::path(get_mi_dir(), {{ folder }}, year, qtr, test)
  }else{
    year_dir <- fs::path(get_mi_dir(), {{ folder }}, year, qtr)
  }
  
  path <- get_dir_path(directory = year_dir, 
                       check_mode = "write")
  
  return(path)
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
get_mi_data_path <- function(type = c("error_data",
                                      "dupe_data",
                                      "clean_data",
                                      "ldp_data",
                                      "final_data",
                                      "wait_data",
                                      "uptake_data",
                                      "comp_data"),
                             ext = c("rds", "csv"), 
                             test_output = FALSE, 
                             previous_data = FALSE,
                             previous_year_to_qt = FALSE) {
  
  if (previous_data){
    file_name <- dplyr::case_match(
      type,
      "error_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_error-summary"),
      "dupe_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_dupes"),
      "clean_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_clean-data"),
      "ldp_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_individuals-with-ldp"),
      "final_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_final-data"),
      "ldp_wait_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_ldp_wait-data"),
      "wait_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_wait-data"),
      "comp_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_comp-data"),
      "subtype_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_subtype-data"),
      "stage_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_stage-data"),
      "model_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_model-data"),
      "uptake_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_uptake-data"),
      "carer_data" ~ stringr::str_glue("{previous_fy}-{previous_qt}_carer-data")
      )
      
  } else if(previous_year_to_qt){
    file_name <- dplyr::case_match(
      type,
      "error_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_error-summary"),
      "dupe_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_dupes"),
      "clean_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_clean-data"),
      "ldp_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_individuals-with-ldp"),
      "final_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_final-data"),
      "ldp_wait_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_ldp_wait-data"),
      "wait_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_wait-data"),
      "comp_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_comp-data"),
      "subtype_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_subtype-data"),
      "stage_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_stage-data"),
      "model_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_model-data"),
      "uptake_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_uptake-data"),
      "carer_data" ~ stringr::str_glue("{as.numeric(fy)-1}-{qt}_carer-data")
    )   
      
  } else {
    file_name <- dplyr::case_match(
    type,
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes"),
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp"),
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data"),
    "ldp_wait_data" ~ stringr::str_glue("{fy}-{qt}_ldp_wait-data"),
    "wait_data" ~ stringr::str_glue("{fy}-{qt}_wait-data"),
    "wait_data_2" ~ stringr::str_glue("{fy}-{qt}_wait-data-2"),
    "comp_data" ~ stringr::str_glue("{fy}-{qt}_comp-data"),
    "subtype_data" ~ stringr::str_glue("{fy}-{qt}_subtype-data"),
    "stage_data" ~ stringr::str_glue("{fy}-{qt}_stage-data"),
    "model_data" ~ stringr::str_glue("{fy}-{qt}_model-data"),
    "uptake_data" ~ stringr::str_glue("{fy}-{qt}_uptake-data"),
    "carer_data" ~ stringr::str_glue("{fy}-{qt}_carer-data")
    )
  }
  
  mi_data_path <- get_file_path(
    directory = get_mi_year_dir("data", test_output = test_output, previous_data = previous_data, previous_year_to_qt = previous_year_to_qt),
    file_name = file_name,
    ext = ext, 
    check_mode = "write"
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
get_mi_output_path <- function(test_output = FALSE, 
                               previous_data = FALSE,
                               previous_year_to_qt = FALSE) {
  
  if (previous_data){
  file_name <- stringr::str_glue("{previous_end_date}_management-report.html")    
  }else if(previous_year_to_qt){
    file_name <- stringr::str_glue("{end_date - years(1)}_management-report.html")    
  }else{
    file_name <- stringr::str_glue("{end_date}_management-report.html")
  }
  
  mi_output_path <- get_file_path(
    directory = get_mi_year_dir("output", test_output = test_output, previous_data = previous_data, previous_year_to_qt = previous_year_to_qt),
    file_name = file_name, 
    check_mode = "write"
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
get_pub_date_dir <- function(folder = c("data", "output"), test_output = FALSE) {
  
  test <- "test"
  
  if ((test_output)){
    pub_date_dir <- fs::path("/","conf","dementia","A&I","Outputs","publication", 
                             {folder}, {pub_date}, test)
  }else{
    pub_date_dir <- fs::path("/","conf","dementia","A&I","Outputs","publication", 
                             {folder}, {pub_date})  
  }
  
  path <- get_dir_path(directory = pub_date_dir, 
                       check_mode = "write")
  
  return(path)
}


#' Return the file path to the finalised publication data 
#' 
#' @description The publication data file is use to produce the annual publication
#'
#' @return the path to the finalised publication data
#'
#'
get_pub_data_path <- function(test_output = FALSE) {
  file_name <- stringr::str_glue("{pub_date}_pub-data")
  
  pub_data_path <- get_file_path(
    directory = get_pub_date_dir("data", test_output = test_output),
    file_name = file_name,
    ext = "rds", 
    check_mode = "write"
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
get_pub_output_path <- function(output_name = c("pub_summary", "pub_report", 
                                                "excel_tables", "discovery_data"),
                                test_output = FALSE) {
  file_name <- dplyr::case_match(
    output_name,
    "pub_summary" ~ stringr::str_glue("{pub_date}_dementia-pds_summary.docx"),
    "pub_report" ~ stringr::str_glue("{pub_date}_dementia-pds_report.docx"),
    "excel_tables" ~ stringr::str_glue("{pub_date}_dementia-pds_excel-tables.xlsx"),
    "discovery_data" ~ stringr::str_glue("{pub_date}_ldp-data.csv")
  )
  
  pub_output_path <- get_file_path(
    directory = get_pub_date_dir("output", test_output = test_output),
    file_name = file_name, 
    check_mode = "write"
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
                                          "c8",
                                          "c9",
                                          "c10",
                                          "c11",
                                          "c12",
                                          "c13",
                                          "summary"), 
                                 test_output = FALSE) {
  
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
    "summary" ~ stringr::str_glue("{pub_date}_summary-chart.png"), 
    "twitter" ~ stringr::str_glue("{pub_date}_dementia-pds_twitter-chart.png")
  )
  
  pub_output_path <- get_file_path(
    directory = fs::path(get_pub_date_dir("output", test_output = test_output), "figures"),
    file_name = file_name, 
    check_mode = "write"
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
    directory = fs::path("/", "conf", "dementia", "A&I", "Outputs", "publication", "templates"),
    file_name = file_name
  )
  
  return(excel_template_file_path)
}


## End of Script ##