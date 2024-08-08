################################################################################
# Name of file - setup_general.R
# Original Authors - Jennifer Thom
# Original Date - August 2024
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Functions to set up reading files from a working directory. Deals
#               with checking for read/write permissions. Find latest file function
#               used for automating the selection of the simd file.
################################################################################


#' Find the latest version of a file
#'
#' @description
#' This will return the latest created file matching
#' the criteria. It uses [fs::dir_info()] to
#' find the files then picks the one with the latest
#' `birthtime`.
#'
#' @param directory The directory in which to search.
#' @param regexp a
#' [regular expression](https://www.regular-expressions.info/quickstart.html)
#' passed to [fs::dir_info()] to search for the file.
#' @param selection_method Valid arguments are "modification_date"
#' (the default) or "file_name".
#'
#' @return the [fs::path()] to the file
#' @export
#'
#' @examples
#' \dontrun{
#' find_latest_file(
#'   directory = get_lookups_dir(),
#'   regexp = "Scottish_Postcode_Directory_.+?\\.rds"
#' )
#' }
find_latest_file <- function(directory,
                             regexp,
                             selection_method = "modification_date") {
  if (selection_method == "modification_date") {
    latest_file <- fs::dir_info(
      path = directory,
      type = "file",
      regexp = regexp,
      recurse = TRUE
    ) %>%
      dplyr::arrange(
        dplyr::desc(.data$birth_time),
        dplyr::desc(.data$modification_time)
      ) %>%
      magrittr::extract(1L, )
    
    n_matched_files <- nrow(latest_file)
    
    if (n_matched_files > 1L) {
      cli::cli_inform(
        c(i = "There were {.val {n_matched_files}} files matching the
                    regexp {.val {regexp}}. {.val {fs::path_file(latest_file$path)}} has been selected,
                    which was modified on {.val {latest_file$modification_time}}.")
      )
    }
  } else if (selection_method == "file_name") {
    latest_file <- fs::dir_info(
      path = directory,
      type = "file",
      regexp = regexp,
      recurse = TRUE
    ) %>%
      dplyr::arrange(
        dplyr::desc(.data$path)
      ) %>%
      magrittr::extract(1L, )
    
    n_matched_files <- nrow(latest_file)
    
    if (n_matched_files > 1L) {
      cli::cli_inform(
        c(i = "There were {.val {n_matched_files}} files matching the
                    regexp {.val {regexp}}. {.val {fs::path_file(latest_file$path)}} has been selected,
                    as it is first alphabetically.")
      )
    }
  }
  
  if (n_matched_files == 1L) {
    cli::cli_alert_info("Using {.val {fs::path_file(latest_file$path)}}.")
  } else {
    cli::cli_abort(
      "There was no file in {.path {directory}} that matched the
        regular expression {.val {regexp}}"
    )
  }
  
  file_path <- latest_file %>%
    dplyr::pull(.data$path)
  
  return(file_path)
}


#' Get and check and full file path
#'
#' @description This generic function takes a directory and
#' file name then checks to make sure they exist.
#' The parameter \code{check_mode} will also test to make sure
#' the file is readable (default) or writeable (\code{check_mode = "write"}).
#' By default it will return an error if the file doesn't exist
#' but with \code{create = TRUE} it will create an empty file with
#' appropriate permissions.
#'
#' @param directory The file directory
#' @param file_name The file name (with extension if not supplied to \code{ext})
#' @param ext The extension (type of the file) - optional
#' @param check_mode The mode passed to
#' [fs::file_access()], defaults to "read"
#' to check that you have read access to the file
#' @param create Optionally create the file if it doesn't exists,
#' the default is to only create a file if we set `check_mode = "write"`
#' @param file_name_regexp A regular expression to search for the file name
#' if this is used `file_name` should not be, it will return the most recently
#' created file using [find_latest_file()]
#' @param selection_method Passed only to [find_latest_file()], will select the
#'  file based on latest modification date (default) or file name
#'
#' @return The full file path, an error will be thrown
#' if the path doesn't exist or it's not readable
#'
#' @family file path functions
#' @export
get_file_path <-
  function(directory,
           file_name = NULL,
           ext = NULL,
           check_mode = "read",
           create = NULL,
           file_name_regexp = NULL,
           selection_method = "modification_date") {
    
    if(!fs::dir_exists(directory) && check_mode != "exists") {
      if (is.null(create) && check_mode == "write" ||
          !is.null(create) && create == TRUE) {
        # The directory doesn't exist but we do want to create it
        fs::dir_create(directory)
        cli::cli_alert_info(
          "The directory {.path {directory}} did not exist, it has now been created."
        )
      } else if(!fs::dir_exists(directory)) {
        return(FALSE)
        cli::cli_abort("The directory {.path {directory}} does not exist.")
      }}
    
    
    check_mode <- match.arg(
      arg = check_mode,
      choices = c("exists", "read", "write", "execute")
    )
    
    
    if (is.null(file_name)) {
      file_path <- fs::path(directory)
    }
    
    if (!is.null(file_name)) {
      file_path <- fs::path(directory, file_name)
    } else if (!is.null(file_name_regexp)) {
      if (check_mode == "read") {
        file_path <- find_latest_file(directory,
                                      regexp = file_name_regexp,
                                      selection_method = selection_method
        )
      } else {
        cli::cli_abort(
          c("{.arg check_mode = \"{check_mode}\"} can't be used to
find the latest file with {.arg file_name_regexp}",
            "v" = "Try {.arg check_mode = \"read\"}"
          )
        )
      }
    } else {
      cli::cli_abort(
        "You must specify a {.var file_name} or a regular expression
                     to search for with {.var file_name_regexp}"
      )
    }
    
    if (!is.null(ext)) {
      file_path <- fs::path_ext_set(file_path, ext)
    }
    
    if (!fs::file_exists(file_path) && check_mode != "exists") {
      if (is.null(create) && check_mode == "write" ||
          !is.null(create) && create == TRUE) {
        # The file doesn't exist but we do want to create it
        fs::file_create(file_path)
        cli::cli_alert_info(
          "The file {.file {fs::path_file(file_path)}} did not exist in
          {.path {directory}}, it has now been created."
        )
      } else {
        possible_file_name <- fs::path_file(
          fs::dir_ls(
            directory,
            regexp = fs::path_ext_remove(file_path),
            ignore.case = TRUE
          )
        )
        
        error_text <- "The file {.file {fs::path_file(file_path)}} does not
        exist in {.path {directory}}"
        
        if (length(possible_file_name) == 1L) {
          # There was a file matching the name, except for case differences.
          error_text <- c(
            error_text,
            ">" = "Did you mean {.file {possible_file_name}}?"
          )
        }
        
        # The file doesn't exist and we don't want to create it
        cli::cli_abort(error_text)
      }
    } else if (check_mode == "exists") {
      if (!fs::file_exists(file_path)) {
        return(FALSE)
      }
    }
    
    if (!fs::file_access(file_path, mode = check_mode)) {
      cli::cli_abort(
        "{.file {fs::path_file(file_path)}} exists in {.path {directory}} but is
        not {check_mode}able."
      )
    }
    
    return(file_path)
  }


#' SIMD File Path
#'
#' @description Get the path to the centrally held Scottish Index of Multiple
#' Deprivation (SIMD) file.
#'
#' @inheritParams get_file_path
#'
#' @return An [fs::path()] to the SIMD file
#' @export
#'
#' @family lookup file paths
get_simd_path <- function(file_name = NULL, ext = "rds") {
  simd_dir <-
    fs::path("/", "conf", "linkage", "output", "lookups", "Unicode", "Deprivation")
  
  simd_path <- get_file_path(
    directory = simd_dir,
    file_name = file_name,
    ext = ext,
    file_name_regexp = stringr::str_glue(
      "postcode_\\d\\d\\d\\d_\\d_simd\\d\\d\\d\\d.*?\\.{ext}"
    )
  )
  
  return(simd_path)
}

### End of Script ###