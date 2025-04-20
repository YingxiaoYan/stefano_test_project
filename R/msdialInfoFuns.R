# ------------------------------------------------------------------------------------------- #
# A collection of functions for `msdialInfo.R`
# ------------------------------------------------------------------------------------------- #

#' Get the matched ID for a output element
#' 
#' @param inputId A character string representing the input ID
#' @returns A string representing the matched ID
#' @export
matched_output_id <- function(inputId) {
  paste0(inputId, "_out")
}

#' Check if the file has a YAML extension
#'
#' @param file Path to the YAML file 
#' @returns TRUE if the file has a YAML extension, otherwise an error is thrown
.check_yaml_file_ext <- function(file) {
  # Check if the file has a YAML extension
  if (!any(c("yml", "yaml") %in% tools::file_ext(file))) {
    stop("The specified `file` doesn't have a YAML extension.")
  }
  TRUE
}

#' Check if all IDs are present in the data information
#'
#' @param ids A vector of IDs to check
#' @param data_info A list of user data information
#' @returns TRUE if all IDs are present, otherwise an error is thrown
.check_all_ids_in_list <- function(ids, data_info) {
  # Check if all IDs are present in the data information
  missing_ids <- setdiff(ids, names(data_info))
  if (length(missing_ids) > 0) {
    stop("The following IDs are missing in the data information: ", 
         paste(missing_ids, collapse = ", "))
  }
  TRUE
}

#' Function to read existing user parameters from a YAML file or create defaults
#'
#' @param file Path to the YAML file where user data information will be saved
#' @param ids A vector of IDs that should be saved in the YAML file
#' @param data_info A list of user data information to be saved
.save_user_input_to_yaml <- function(file, ids, data_info) {
  stopifnot(is.list(data_info))
  .check_yaml_file_ext(file)
  .check_all_ids_in_list(ids, data_info)
  data_info <- data_info[names(data_info) %in% ids]
  yaml::write_yaml(data_info, file = file)
}

#' Function to handle saving user data information
#' 
#' @param file Path to the YAML file where user data information will be saved
#' @param ids A vector of IDs that should be saved in the YAML file
#' @param data_info A list of user data information to be saved
#'
#' @export
save_user_data_info <- function(file, ids, data_info) {
  stopifnot(is.list(data_info))
  stopifnot(! shiny::is.reactive(data_info))
  .save_user_input_to_yaml(file, ids, data_info) 
}

#' Read or create parameters from a YAML file
#'
#' @param file Path to the YAML file
#' @param ids A vector of parameter names to be read from the YAML file
#' @param defaults A list of default parameters to be used if the YAML file does not exist
#'
#' @returns A list of parameters read from the YAML file or the default parameters
#' @export
read_or_create_params <- function(file, ids, defaults) {
  .check_yaml_file_ext(file)
  if (file.exists(file)) {
    y <- yaml::read_yaml(file)
    .check_all_ids_in_list(ids, y)
    return(y)
  } else {
    # Create the YAML file, which is required.
    .save_user_input_to_yaml(file, ids, defaults)
    warning(file, "YAML file not found.",
            "A new YAML file has been created with default parameters.\n")
    return(defaults)
  }
}


#' Get the file/directory name from a [shinyFiles::shinyFilesButton()] or
#' [shinyFiles::shinyDirButton()]
#' 
#' @param button The button input from the user, `input[[button_id]]`
#' @param default The default file/directory name to return if the button is not clicked
#' @param roots The roots for the file/directory selection
#' 
#' @returns A character string representing the file/directory name
#'
#' @md
#' @export
getShinyFileName <- function(button, default, roots) {
  if (is.numeric(button)) {
    return(default)
  } else {
    return(shinyFiles::parseFilePaths(roots, button)$datapath)
  }
}
#' @rdname getShinyFileName
#' @alias getShinyFileName
#' @export
getShinyDirName <- function(button, default, roots) {
  if (is.numeric(button)) {
    return(default)
  } else {
    return(shinyFiles::parseDirPath(roots, button))
  }
}



