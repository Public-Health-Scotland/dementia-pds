################################################################################
# Name of file - render_check.R
# Original Authors - Abram McCormick
# Original Date - August 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Function to run render::markdown that first checks for an existing file. If a file already exists 
# then permission to overwrite the file must be given via the console.
################################################################################

render_check <- function(input, output_file, menu_input = 2, ...) {
  
  # If the file already exists, display a warning message asking for user input (1 to overwrite or 0 to abort)
  if (fs::file_exists(output_file)) {
    # If the file was not created by the current user, add this information to the warning
    if (fs::file_info(output_file)$user != Sys.getenv("USER")){
      input <- menu(
        c("yes, overwrite the file (enter 0 to abort)"), 
        title = cli::cli_alert_info("The file {.file {fs::path_file(output_file)}} already exists and was created by another user, are you sure you want to overwrite the file?")
      )
    }
    input <- menu(
      c("yes, overwrite the file (enter 0 to abort)"), 
      title = cli::cli_alert_info("The file {.file {fs::path_file(output_file)}} already exists, are you sure you want to overwrite the file?")
    )
  }
  
  # If the file does not exist or user input is 1, render the report
  # The report is first rendered to /conf/dementia as a work-around since phs_report_docx errors when the file path contains "&"
  if (!fs::file_exists(output_file) | menu_input == 1) {
    rmarkdown::render(
      input = input,
      output_file = "/conf/dementia/temp.docx")
    fs::file_move(
      "/conf/dementia/temp.docx",
      output_file
    )
  }
  
  # If the user input is 1, display a message to say the file has been overwritten
  if (input == 1) {
    cli::cli_alert_info("The file {.file {fs::path_file(output_file)}} has been overwritten.")
  # If the user input is 0, return an error message
  } else if (input == 0) {
    cli::cli_abort(
      message = "The file {.file {fs::path_file(output_file)}} already exists and has NOT been overwritten. Re-run the section above and enter 1 in the console to overwrite file."
    )
  }
}
