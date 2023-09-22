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


# Create data folder for FY and Qtr
mi_data_path <- function(type = c("error_data", 
                                  "dupe_data", 
                                  "clean_data", 
                                  "ldp_data", 
                                  "final_data"
),
ext = c("rds", 
        "csv")
) {
  year_dir <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
  mi_dir <- dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "data", {year_dir}))
  file_name <- file_name <- dplyr::case_match(
    type,
    "error_data" ~ stringr::str_glue("{fy}-{qt}_error-summary"),
    "dupe_data" ~ stringr::str_glue("{fy}-{qt}_dupes"), 
    "clean_data" ~ stringr::str_glue("{fy}-{qt}_clean-data"),
    "ldp_data" ~ stringr::str_glue("{fy}-{qt}_individuals-with-ldp"), 
    "final_data" ~ stringr::str_glue("{fy}-{qt}_final-data")
  )
  mi_path <- stringr::str_glue("{mi_dir}/{file_name}.{ext}")
  
  return(mi_path)
}


# Create MI output folder
mi_output_path <- function(){
  
  year_dir <- stringr::str_glue("{fy}-{substr(as.numeric(fy)+1, 3, 4)}/Q{qt}")
  mi_dir <- dir_create(path("/", "conf", "dementia", "A&I", "Outputs", "management-report", "output", {year_dir}))
  file_name <- stringr::str_glue("{end_date}_management-report.html")
  mi_output_path <- stringr::str_glue("{mi_dir}/{file_name}")
  
  return(mi_output_path)
}


