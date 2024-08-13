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
  if (file.size(path) != 0) {
    input <- menu(c("yes, overwrite the file (enter 0 to abort)"), title = cli::cli_alert_info("The file {.file {fs::path_file(path)}} already exists, are you sure you want to overwrite the file?"))
  }
  if (file.size(path) == 0 | input == 1) {
    valid_extensions <- c("rds", "csv")
    ext <- fs::path_ext(path)
    if (!(ext %in% valid_extensions)) {
      cli::cli_abort(c(
        "x" = "Invalid extension: {.val {ext}}",
        "i" = "{.fun read_file} supports {.val {valid_extensions}}"
      ))
    }
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
    if (fs::file_info(path)$user == Sys.getenv("USER")) {
      # Set the correct permissions
      fs::file_chmod(path = path, mode = "770")
      fs::file_chown(path = path, group_id = "3182")
    }
  }
    if (input == 1) {
      cli::cli_alert_info("The file {.file {fs::path_file(path)}} has been overwritten.")
      return(invisible(data))
    }
    if (input == 2) {
      return(invisible(data))
  }
   if (input == 0) {
      cli::cli_abort(message =
        "The file {.file {fs::path_file(path)}} already exists and has NOT been overwritten. Re-run the section above and enter 1 in the console to overwrite file."
      )
    }
}