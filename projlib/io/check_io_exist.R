#' Check all files exist
#'
#' @param fns file names. or list of list of ... files
#' @export
chq_all_files_exist <- function(fns) {
  fns <- unlist(fns)
  ex <- file.exists(fns)
  if(! all(ex)) {
    files <- paste(fns[!ex], "\n")
    stop(paste("\nThese files are required but not exist.\n", files))
  }
  invisible(TRUE)
}

#' Make directory if not exists
#' 
#' @param dirs directory names, or list of list of ... directories
#' @export
mkdir_if_not_exist <- function(dirs) {
  dirs <- unique(unlist(dirs))   # Remove duplicates
  missing <- !dir.exists(dirs)
  if(any(missing)) {
    warning(paste("These directories are not exist. Creating them.\n", dirs[missing]))
    was_successful <- sapply(dirs[missing], \(.x) dir.create(.x, recursive = TRUE))
    return(invisible(all(was_successful)))
  } else {
    for(dir in dirs) {
      if (file.access(dir, 2L) != 0) stop("Write permission is required to", dir)
    }
    return(invisible(TRUE))
  }
}

#' Check input files and output directories
#' 
#' @param FILE a list that contains input files in `i` and output in `o`
#' @return a logical vector for input and output existence
#' @export
check_io_exist <- function(FILE) {
  i <- if (!is.null(FILE$i)) {
    chq_all_files_exist(FILE$i)  # brief check if all input files exist
  } else {
    TRUE
  }
  o <- if (!is.null(FILE$o)) {
    mkdir_if_not_exist(dirname(unlist(FILE$o)))
  } else {
    TRUE
  }
  invisible(c(i, o))
}
