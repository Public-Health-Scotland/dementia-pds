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
  if (file.size(output_file) != 0) {
    menu_input <- menu(c("yes, overwrite the file (enter 0 to abort)"), title = cli::cli_alert_info("The file {.file {fs::path_file(output_file)}} already exists, are you sure you want to overwrite the file?"))
  }
  if (file.size(output_file) == 0 | menu_input == 1) {
    rmarkdown::render(
      input = input,
      output_file = output_file)
  }
  if (menu_input == 1) {
    cli::cli_alert_info("The file {.file {fs::path_file(output_file)}} has been overwritten.")
      }
  if (menu_input == 2) {
      }
  if (menu_input == 0) {
    cli::cli_abort(message =
       "The file {.file {fs::path_file(output_file)}} already exists and has NOT been overwritten. Re-run the section above and enter 1 in the console to overwrite file."
    )
  }
}