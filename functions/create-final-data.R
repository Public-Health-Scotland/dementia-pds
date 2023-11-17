#########################################################################
# Name of file - create-final-data.R
# Data release - Quarterly Dementia PDS Management Reports
# Original Authors - Alice Byers
# Original Date - September 2020
#
# Written/run on - RStudio Desktop
# Version of R - 3.6.1
#
# Description - Create finalised data file for years no longer submitted.
#########################################################################

create_final_data <- function(fy_final, collated_file){
  
  `%>%` <- magrittr::`%>%`
  
  if(!file.exists(collated_file)){
    stop(paste(collated_file, "does not exist."))
  }

  if(!stringr::str_detect(fy_final, "^\\d{4}\\/\\d{2}$")){
    stop("fy_final must be in YYYY/YY format.")
  }
  
  if(as.numeric(stringr::str_sub(fy_final, 6, 7)) != 
      as.numeric(stringr::str_sub(fy_final, 3, 4)) + 1){
    stop("fy_final is not a valid financial year.")
  }
  
  start_final <- lubridate::dmy(paste0("0104", 
                                       stringr::str_sub(fy_final, 1, 4)))
  end_final   <- lubridate::dmy(paste0("310320", 
                                       stringr::str_sub(fy_final, 6, 7)))
  
  pds <-
    
    readr::read_csv(collated_file, col_types = cols(.default = "c")) %>%
    
    janitor::clean_names() %>%
    
    # Convert dates from character to date format
    dplyr::mutate(dplyr::across(tidyselect::contains("date"), 
                                lubridate::ymd)) %>%
    
    # Pad CHI Number to 10 digits
    dplyr::mutate(chi_number = phsmethods::chi_pad(chi_number)) %>%
    
    # Replace word 'and' with ampersand
    dplyr::mutate(health_board = 
                    stringr::str_replace(health_board, " and ", " & ")) %>%
    
    # Extract records with diagnosis date before start date
    dplyr::filter(dplyr::between(dementia_diagnosis_confirmed_date, 
                                 start_final, end_final))
  
  final_file <- paste0(final_data_path(), glue::glue("{stringr::str_replace(fy_final, '/', '-')}",
                          "_final-data.rds"))
  
  # Check if file already exists
  if(file.exists(final_file)){
    
    stop(glue::glue("A finalised data file for {fy_final} already exists at ",
               "{final_file}."))
    
  }else{
    
    print(glue("Saving final file for {fy_final}..."))
    
    # Save final file
    readr::write_rds(
      pds,
      final_file,
      compress = "gz"
    )
    
    print(glue::glue("File saved to {final_file}"))
    
  }
    
}


### END OF SCRIPT ###