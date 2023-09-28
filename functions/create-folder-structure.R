# Create folder structure functions 

# Path to final files
collated_file_path <- function(){
  
  collated_file_dir <- path("/", "conf", "dementia", "03-Outputs", "National")
  file_name <- stringr::str_glue("{fy}-Q{qt}_national.csv")
  collated_file_path <- stringr::str_glue("{collated_file_dir}/{file_name}")
  
  return(collated_file_path)
}


# Path to final files
final_data_path <- function(){
  
  final_data_dir <- dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", "final"))
  
  return(final_data_dir)
}


pub_figures_path <- function(){
  
  figures_dir <- fs::dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "publication", "output", {pub_date}, "figures"))
  
  return(figures_dir)
}


# setup directory
setup_dir <- function(directory = c("mi", "publication"), 
                      folder = c("data", "output")
                      ){
  
  year_dir <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
  
  if(directory == "mi"){
    
    dir <- fs::dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", {folder}, {year_dir}))
  }
  
  if(directory == "publication"){
    
    dir <- fs::dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "publication", {folder}, {pub_date}))
  }
  
  return(dir)
}


# Create data directories for MI report and publication
data_path <- function(directory = c("mi", "publication"),
                      type = c("error_data", 
                               "dupe_data", 
                               "clean_data", 
                               "ldp_data", 
                               "final_data", 
                               "pub_data"),
                      ext = c("rds", "csv")) {
  
  
  dir <- setup_dir(directory, "data")
  
  file_name <- file_name <- dplyr::case_match(
    type,
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes"), 
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp"), 
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data"), 
    "pub_data" ~ stringr::str_glue("{pub_date}_pub-data")
  )
  
  path <- stringr::str_glue("{dir}/{file_name}.{ext}")
  
  return(path)
}


# Create output folder - parameter for MI and publication outputs
output_path <- function(directory = c("mi", "publication"),
                        output_name = c("mi_report", "pub_summary", "pub_report")
                        ){
    
    dir <- setup_dir(directory, "output")
  
  file_name <- file_name <- dplyr::case_match(output_name,
    "mi_report" ~ stringr::str_glue("{end_date}_management-report.html"),
    "pub_summary" ~ stringr::str_glue("{pub_date}_dementia-pds_summary.docx"), 
    "pub_report" ~ stringr::str_glue("{pub_date}_dementia-pds_report.docx"))

  output_path <- stringr::str_glue("{dir}/{file_name}")
  
  return(output_path)
}


