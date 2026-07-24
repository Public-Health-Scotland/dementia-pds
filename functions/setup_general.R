################################################################################.
# Name of file - setup_general.R
# Original Authors - Jennifer Thom
# Original Date - August 2024
# Updated By - Lucy Binsted
# Date - July 2026
#
# Written/run on - RStudio Server
# Version of R - 4.1.2
#
# Description - Functions to set up reading files from a working directory. 
#               Deals with checking for read/write permissions. Find latest file 
#               function used for automating the selection of the SIMD file.
################################################################################.

#' Find a file matching a regular expression
#'
#' @description
#' Searches a directory for files matching a regular expression and returns
#' a single file path.
#'
#' If multiple files are found, one is selected according to
#' `selection_method`.
#'
#' @param directory A directory to search.
#' @param regexp A regular expression passed to [fs::dir_info()] to identify
#' files of interest.
#' @param selection_method Character string specifying how a file should be
#' selected when multiple matches are found. One of:
#' \describe{
#'   \item{`"modification_date"`}{Select the file with the most recent
#'   modification time. If multiple files share the same modification time,
#'   the file that is last alphabetically is selected.}
#'   \item{`"file_name"`}{Select the file that is last alphabetically.}
#' }
#' @param recurse Logical. Should files in nested directories also be searched?
#' Defaults to `FALSE`.
#'
#' @return An [fs::path()] object containing the selected file path.
#'
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
                             selection_method = "modification_date",
                             recurse = FALSE) {
  
  # Check the selection method argument
  selection_method <- match.arg(
    selection_method,
    choices = c("modification_date", "file_name")
  )
  
  # Search the directory for files matching the regular expression
  matches <- fs::dir_info(
    path = directory,
    type = "file",
    regexp = regexp,
    recurse = recurse
  )
  
  # If no files are found, return an error message
  if (nrow(matches) == 0L) {
    cli::cli_abort(
      "There were no files in {.path {directory}} that matched the regular expression {.val {regexp}}."
    )
  }
  
  # If one file is found, return it's path with an info message
  if (nrow(matches) == 1L) {
    cli::cli_alert_info(c(
      "There was one file in {.path {directory}} that matched the regular expression {.val {regexp}}.",
      " Using {.file {fs::path_file(matches$path)}}."
    ))
    return(matches$path)
  }
  
  # If multiple files are found, choose one based on the selection method
  n_results <- nrow(matches)
  msg <- c(
    "There were {.val {n_results}} files in {.path {directory}} that matched the regular expression {.val {regexp}}."
  )
  
  # If the selection method is `modification_date`, select the files with the latest modification time
  if (selection_method == "modification_date") {
    matches <- matches %>%
      dplyr::filter(.data$modification_time == max(.data$modification_time))
    
    # If there is one file with the latest modification date, return it's path with an info message
    if (nrow(matches) == 1L) {
      cli::cli_alert_info(c(
        msg, 
        " {.file {fs::path_file(matches$path)}} has been selected based on modification date ({.val {matches$modification_time}})."
      ))
      return(matches$path)
    } 
    
    # If there are multiple files with the latest modification date, or the selection method is `file_name`, select the file that is last alphabetically (typically corresponding to the highest numbered file)
    matches <- matches %>%
      dplyr::arrange(dplyr::desc(fs::path_file(.data$path))) %>%
      dplyr::slice(1L)
    cli::cli_alert_info(c(
      msg,
      " Multiple files shared the latest modification date.",
      " {.file {fs::path_file(matches$path)}} has been selected as it is last alphabetically."
    ))
    return(matches$path)
  }
  
  # Select by file name (last alphabetically)
  matches <- matches %>%
    dplyr::arrange(dplyr::desc(fs::path_file(.data$path))) %>%
    dplyr::slice(1L)
  cli::cli_alert_info(c(
    msg,
    " {.file {fs::path_file(matches$path)}} has been selected as it is last alphabetically (typically corresponding to the highest numbered file)."
  ))
  return(matches$path)
}


#' Check a directory path
#'
#' @description
#' Validates a directory path and optionally creates the directory if it does
#' not already exist.
#'
#' The `check_mode` argument is passed to [fs::file_access()] to verify that
#' the directory can be accessed as required. By default, the function checks
#' that the directory exists and is readable.
#'
#' @param directory A directory path.
#' @param check_mode The access mode passed to [fs::file_access()]. One of
#' `"read"`, `"write"` or `"exists"`. Defaults to `"read"`.
#' @param create Logical. If `TRUE`, create the directory (and any missing
#' parent directories) if it does not already exist. Defaults to `FALSE`.
#'
#' @return An [fs::path()] object containing the validated directory path.
#'
#' @export

check_dir_path <- function(directory,
                           check_mode = "read",
                           create = FALSE) {
  
  # Check the check mode argument
  check_mode <- match.arg(
    check_mode,
    c("exists", "read", "write")
  )
  
  # If the directory does not exist, create it or return error message
  if (!fs::dir_exists(directory)) {
    # Create directory if requested
    if (isTRUE(create)) {
      directory <- fs::dir_create(
        directory,
        mode = "u=rwx,go=rwx"
      )
      cli::cli_alert_info(
        "The directory {.path {directory}} did not exist and has now been created."
      )
    # Return error message
    } else {
      cli::cli_abort(
        "The directory {.path {directory}} does not exist."
      )
    }
  }
  
  # If the directory cannot be accessed, return an error message
  if (!fs::file_access(path = directory, mode = check_mode)) {
    cli::cli_abort(
      "The directory {.path {directory}} exists but is not {check_mode}able."
    )
  }
  
  # If the directory exists and can be accessed, return it
  return(fs::path(directory))
}
      

#' Check a file path
#'
#' @description
#' Constructs and validates a file path.
#'
#' The function checks that the directory exists and can be accessed with the
#' required permissions. It then constructs a file path from `file_name`, or
#' locates a file using `file_name_regexp`.
#'
#' For `check_mode = "read"`, the file must exist and be readable.
#'
#' For `check_mode = "write"`, the directory must be writable. The file may
#' already exist or may be created later by the calling function.
#'
#' @param directory A directory path.
#' @param file_name A file name. If `ext` is supplied, the extension will be
#' added or replaced.
#' @param ext Optional file extension.
#' @param check_mode The access mode required for the file. One of `"read"` or
#' `"write"`. Defaults to `"read"`.
#' @param create_dir Logical. If `TRUE`, create the directory (and any missing
#' parent directories) if it does not already exist.
#' @param file_name_regexp A regular expression used to locate a file. If
#' supplied, `file_name` must be `NULL`.
#' @param selection_method Passed to [find_latest_file()] when
#' `file_name_regexp` is used. One of `"modification_date"` or `"file_name"`.
#'
#' @return An [fs::path()] object containing the validated file path.
#'
#' @family file path functions
#' @export

check_file_path <- function(directory,
                            file_name = NULL,
                            ext = NULL,
                            check_mode = "read",
                            create_dir = FALSE,
                            file_name_regexp = NULL,
                            selection_method = "modification_date",
                            recurse = FALSE) {
    
    # Check the directory exists with the required permissions, and create it if requested
    directory <- check_dir_path(directory, check_mode, create_dir)
    
    # 1. If both a file name and regular expression are provided, return an error message
    if (!is.null(file_name) && !is.null(file_name_regexp)) {
      cli::cli_alert_info(
        "Specify only one of {.arg file_name} or {.arg file_name_regexp}."
      )
    }
    
    # 2. If a file name is provided, use it to get the file path
    if (!is.null(file_name)) {
      file_path <- fs::path(directory, file_name)
      # 2.1. If a file extension is provided, add it to the file path
      if (!is.null(ext)) {
        file_path <- fs::path_ext_set(file_path, ext)
      }
    # 3. If a regular expression is provided, use find_latest_file to get the file path
    } else if (!is.null(file_name_regexp)) {
      # 3.1. If check mode is read find_latest_file can be used
      if (check_mode == "read") {
        file_path <- find_latest_file(
          directory,
          regexp = file_name_regexp,
          selection_method = selection_method,
          recurse = recurse
        )
      # 3.2. If check mode is not read, return an error message
      } else {
        cli::cli_abort(c(
          "{.arg check_mode = \"{check_mode}\"} can't be used to find the latest file with {.arg file_name_regexp}", 
          "v" = "Try {.arg check_mode = \"read\"}"
        ))
      }
      
    # 4. If neither a file name or regular expression is provided, return an error message
    } else {
      cli::cli_abort(
        "You must specify a {.var file_name} or a regular expression to search for with {.var file_name_regexp}"
      )
    }
    
    # 5. If the file does not exist, look at the check mode
    if (!fs::file_exists(file_path)) {
      # 5.1. If the check mode is write, return the file path 
      if (check_mode == "write"){
        return(fs::path(file_path))
      # 5.2. If the check mode is read or exists, look for similar files
      } else {
        error_text <- "The file {.file {fs::path_file(file_path)}} does not exist in {.path {directory}}."
        possible_file_name <- fs::path_file(fs::dir_ls(
          directory,
          regexp = stringr::str_replace_all(
            fs::path_ext_remove(fs::path_file(file_path)),
            "([.|()\\[\\]{}+*?^$\\\\])",
            "\\\\\\1"), 
          ignore.case = TRUE
        ))
        # 5.3. If there was one similar file name, return it with an error message
        if (length(possible_file_name) == 1L) {
          cli::cli_abort(c(
            error_text,
            "i" = "Did you mean {.file {possible_file_name}}?"
          ))
        # 5.4. If there was more than one similar file name, return them with an error message
        } else if (length(possible_file_name) > 1L) {
          cli::cli_abort(c(
            error_text,
            "i" = "Possible matches:",
            setNames(possible_file_name, rep("*", length(possible_file_name)))
          ))
        # 5.5. If there were no similar file names, return an error message
        } else {
          cli::cli_abort(error_text)
        }
      }
        
    # 6. If the file exists, check access
    } else {
      # 6.1. If the file cannot be accessed, return an error message
      if (!fs::file_access(path = file_path, mode = check_mode)) {
        cli::cli_abort(
          "{.file {fs::path_file(file_path)}} exists in {.path {directory}} but is not {check_mode}able."
        )
      # 6.2. If the file can be accessed, return the file path  
      } else {
        return(fs::path(file_path))
      }
    }
}

################################ End of Script #################################.