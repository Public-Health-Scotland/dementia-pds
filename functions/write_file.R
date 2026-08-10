################################################################################
# Name of file - write_file.R
# Original Authors - Jennifer Thom
# Original Date - June 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Function to write to disk and set the correct permissions.  
################################################################################

write_file <- function(data, path, input = 2, ...) {
  
  # If the file already exists, display a warning message asking for user input (1 to overwrite or 0 to abort)
  if (fs::file_exists(path)) {
    # If the file was not created by the current user, add this information to the warning
    if (fs::file_info(path)$user != Sys.getenv("USER")){
      input <- menu(
        c("yes, overwrite the file (enter 0 to abort)"), 
        title = cli::cli_alert_info("The file {.file {fs::path_file(path)}} already exists and was created by another user, are you sure you want to overwrite the file?")
      )
    }
    input <- menu(
      c("yes, overwrite the file (enter 0 to abort)"), 
      title = cli::cli_alert_info("The file {.file {fs::path_file(path)}} already exists, are you sure you want to overwrite the file?")
    )
  }
  
  # If the file does not exist or user input is 1, check the extension then write the file
  if (!fs::file_exists(path) | input == 1) {
    valid_extensions <- c("rds", "csv")
    ext <- fs::path_ext(path)
    # If the extensions provided are not valid, return error message
    if (!(ext %in% valid_extensions)) {
      cli::cli_abort(c(
        "x" = "Invalid extension: {.val {ext}}",
        "i" = "{.fun read_file} supports {.val {valid_extensions}}"
      ))
    }
    # Write file with function depending on extension
    switch(ext,
           "rds" = readr::write_rds(
             x = data,
             file = path,
             compress = "xz",
             version = 3L,
             ...,
             compression = 9L
           ),
           "csv" = readr::write_csv(
             x = data,
             file = path,
             ...
           )
    )
    # Set the correct permissions
    if (fs::file_info(path)$user == Sys.getenv("USER")) {
          fs::file_chmod(path = path, mode = "770")
          fs::file_chown(path = path, group_id = 3182)
    }
  }
  
  # If the user input is 1, display a message to say the file has been overwritten
  if (input == 1) {
      cli::cli_alert_info("The file {.file {fs::path_file(path)}} has been overwritten.")
      return(invisible(data))
  # If the user input is 2, (i.e. default meaning no user input as the file did not exist), don't display a message
  } else if (input == 2){
    return(invisible(data))
  # If the user input is 0, return an error message
  } else if (input == 0) {
      cli::cli_abort(message =
        "The file {.file {fs::path_file(path)}} already exists and has NOT been overwritten. Re-run the section above and enter 1 in the console to overwrite file."
      )
  }
}
