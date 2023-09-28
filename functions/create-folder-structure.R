# Create folder structure functions 

# Path to final files
collated_file_path <- function(){
  
  collated_file_dir <- path("/", "conf", "dementia", "03-Outputs", "National")
  file_name <- stringr::str_glue("{fy}-Q{qt}_national.csv")
  collated_file_path <- stringr::str_glue("{collated_file_dir}/{file_name}")
  
  return(collated_file_path)
}

'/conf/dementia/A&I/Outputs/publication/aberdeen_city_lookup.xlsx'

# Path to Aberdeen city lookup
ac_lookup_path <- function(){
  
  ac_lookup_dir <- path("/", "conf", "dementia", "A&I", "Outputs", "publication", "lookups")
  file_name <- stringr::str_glue("aberdeen_city_lookup.xlsx")
  ac_lookup_path <- stringr::str_glue("{ac_lookup_dir}/{file_name}")
  
  return(ac_lookup_path)
}

# Path to final files
final_data_path <- function(){
  
  final_data_dir <- dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", "final"))
  
  return(final_data_dir)
}

exp_diagnoses <- 

pub_figures_path <- function(type = c("c1",
                                      "c2", 
                                      "c3", 
                                      "c4", 
                                      "c5", 
                                      "c6", 
                                      "c7", 
                                      "summary")
                                      ){
  
  figures_dir <- fs::dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "publication", "output", {pub_date}, "figures"))
  
  file_name <- file_name <- dplyr::case_match(
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
  
  path <- stringr::str_glue("{figures_dir}/{file_name}")
  
  return(path)
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


