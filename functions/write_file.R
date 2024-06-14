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

write_file <- function(data, path, ...) {
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
    fs::file_chmod(path = path, mode = "660")
  }
  
  return(invisible(data))
}