
# Read MS-DIAL Files ---------------------------------------------------------------------

#' Identify three column sections in the MS-DIAL output file
#' 
#' @param msdial_file Path to the MS-DIAL output file
#' 
#' @return A list of three column indices
#' @export
get_three_section_indices <- function(msdial_file) {
  # Read the second (`File type`) line to identify three sections
  file_type_line <- utils::read.delim(
    msdial_file, header = FALSE, nrow = 5L, na.strings = c("NA", "")
  )[2, ]
  i_2nd <- which(! is.na(file_type_line))    # Not NA is the 2nd column sections
  stopifnot(file_type_line[i_2nd[1L]] == "File type")    # As expected
  list(
    "1st" = 1L:i_2nd[1L],
    "2nd" = (i_2nd[1L] + 1L):max(i_2nd),         # "+ 1L" for the title column of the rows
    "3rd" = (max(i_2nd) + 1L):length(file_type_line)
  )
}

#' Fetch sample information from the first 5 lines
#'
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the columns that contain sample information
#' 
#' @return A data frame with sample information
#' @export
fetch_sample_info <- function(msdial_file, indices) {
  if (missing(indices)) indices <- get_three_section_indices(msdial_file)[[2]]
  # Read sample information lines (first 5 lines)
  sample_lines <- utils::read.delim(
    msdial_file, 
    header = FALSE,
    nrow = 5L,
    na.strings = c("NA", "")
  )
  header <- sample_lines[, indices[1L] - 1L]
  
  # Extract sample information  
  sinfo <- sample_lines[, indices, drop = FALSE] |> 
    t() |> 
    as.data.frame()
  colnames(sinfo) <- header          # There was no header in `sinfo` before
  stopifnot(
    "Deviation from the expected names for samples" =
      identical(
        colnames(sinfo)[-5], 
        c("Class", "File type", "Injection order", "Batch ID")
      )
  )
  colnames(sinfo)[5] <- "sample_name"
  # Syntactically valid for variable name in R
  rownames(sinfo) <- make.names(sinfo$sample_name, unique = TRUE)
  sinfo <- sinfo |> 
    dplyr::select(
      Class,
      sample_type = "File type",
      injection_order = "Injection order",
      sample_name,
    )
  # For plot and table
  sinfo <- sinfo |> 
    labelled::set_variable_labels(
      Class = "Class",
      sample_type = "Sample Type",
      injection_order = "Injection Order",
      sample_name = "Sample ID",
    )
  sinfo
}

#' Fetch data of the selected columns of the MS-DIAL output file
#'
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the selected columns
#' 
#' @return A tibble 
#' @export
fetch_data_of_columns <- function(msdial_file, indices) {
  stopifnot(!missing(indices))
  # Count total number of columns in the file
  l1 <- readr::read_tsv(msdial_file, skip = 4L, n_max = 1L, col_names = FALSE, col_types = "c")
  coltypes <- rep("-", ncol(l1))      # Skip the rest columns
  coltypes[indices] <- "c"
  # Read lines of the selected columns
  readr::read_tsv(
    msdial_file, 
    skip = 4L,      # Data starts from the 5th line
    col_types = paste0(coltypes, collapse = ""),
    name_repair = "unique_quiet"
  )
}




#' Confirm the `params` list has the required parameters
#' 
#' @param params A list of parameters
#' @param ... The required parameters in a character vector
#' @return TRUE if the required parameters are present
#' @export
has_required_params <- function(params, ...) {
  for (p in c(...)) {
    if(is.null(params[[p]])) stop(paste(p, "is required"))
  } 
}

#' Make directory if not exists
#' 
#' If the directory does not exist, it will be created with the specified mode.
#' Write permission is required to the directory.
#' 
#' @param dir A directory path
#' @param mode The mode of the creating directory, when it does not exist
#' @return The directory path
#' @export
mkdir_if_not_exist <- function(dir, mode = "0777") {
  stopifnot(is.character(dir) && length(dir) == 1L)
  dir_q <- deparse1(substitute(dir))
  if(!dir.exists(dir)) {
    warning(paste("The directory", dir_q, "(", dir, ") does not exist. Creating it now."))
    is_created <- dir.create(dir, showWarnings = FALSE, recursive = TRUE, mode = mode)
    stopifnot("The directory could not be created" = is_created)
  }
  if (file.access(dir, 2L) != 0) stop("Write permission is required to", dir_q, "(", dir, ")")
  invisible(dir)
}

#' Get the file name of the parsed data
#'
#' @param params A list of parameters including `input_file` and `intermediate_dir`.
#' @param suffix A suffix to add to the file name to distinguish between data at different
#'   stages, such as raw and processed data.
#'
#' @return A text string of the file name
#' @export
get_sumexp_file_name <- function(params, suffix = '') {
  has_required_params(params, "input_file", "intermediate_dir")
  file <- params$input_file
  dir <- mkdir_if_not_exist(params$intermediate_dir)
  # Expected output file name from `read-msdial.R`
  file.path(dir, basename(file)) |> 
    tools::file_path_sans_ext(compression = TRUE) |> 
    paste0(suffix, ".rds")
}

#' Read the parsed MS-DIAL data
#'
#' @param params A list of parameters including `input_file` and `intermediate_dir`.
#'
#' @return A SumExp object
#' @export
read_parsed_msdial_data <- function(params) {
  has_required_params(params, "input_file")
  sumexp_file <- get_sumexp_file_name(params)
  stopifnot("Run `read-msdial.R first" = file.exists(sumexp_file))
  # Load the parsed data
  sumexp <- readRDS(sumexp_file)
  # Confirm the input file is the same as the one used in the parsing
  m5_f <- digest::digest(params$input_file, algo = "md5", file = TRUE)
  m5_se <- SumExp::metadata(sumexp)$file_md5    # Saved by `read-msdial.R`
  if (m5_f != m5_se) {
    stop("The input file is different from the one used in the parsing.",
         "Please re-run `read-msdial.R` first.")
  }
  return(sumexp)
}


# Export Data ----------------------------------------------------------------------------

#' Find the number of decimal places for rounding
.find_rounding_decimal_places <- function(x) {
  x <- abs(x)
  x[x == 0] <- NA
  logx <- log10(x)
  m <- stats::median(logx, na.rm = TRUE)
  max(0, -floor(m) + 4)
}

#' Export data with the feature table to a tab-separated file
#'
#' @param sumexp A SumExp object
#' @param mat_id The name of the data in `sumexp` (or assay) to be exported
#' @param in_file Path to the MS-DIAL output file that has been used to create `sumexp`.
#'   Unsaved feature information will be copied from this file.
#' @param out_file Path to the output tab-separated file
#' @export
export_data_with_feature_table_tsv <- function(sumexp, mat_id, in_file, out_file) {
  # Prepare the feature table
  i_sec <- get_three_section_indices(in_file)
  intact_feature_cols <- fetch_data_of_columns(in_file, i_sec[[1]])
  # Prepare the data table
  mat_x <- sumexp[[mat_id]]
  df_x <- mat_x |>  
    round(.find_rounding_decimal_places(mat_x)) |>     # Reduce the number of decimals
    tibble::as_tibble()
  # Original sample ID
  colnames(df_x) <- SumExp::col_df(sumexp)$sample_name
  df_x <- df_x |> 
    dplyr::mutate("Alignment ID" = SumExp::row_df(sumexp)$alignment_id, .before = 1) |> 
    dplyr::right_join(intact_feature_cols, y = _, by = "Alignment ID")
  readr::write_tsv(df_x, file = out_file, na = "")
}

#' Export the concentration table
#'
#' @param sumexp A SumExp object to be exported
#' @param file Path to the output tab-separated file
#' @export
export_concentration_tsv <- function(sumexp, file) {
  methods::validObject(sumexp)     # Check consistency between elements, eg row_df, col_df, conc
  # The first three rows are the sample information
  s_rows <- SumExp::col_df(sumexp) |> 
    dplyr::select(
      "Class" = "Class",
      "Sample Type" = "sample_type",
      "Injection Order" = "injection_order",
    )
  s_rows_tr <- data.frame(
    matrix(nrow = ncol(s_rows), ncol = 4),    # Feature columns
    t(s_rows)
  )
  s_rows_tr[4] <- colnames(s_rows)
  readr::write_tsv(s_rows_tr, file, append = FALSE, col_names = FALSE, na = "")
  
  # Prepare the feature table
  feature_columns <- SumExp::row_df(sumexp) |> 
    tibble::as_tibble() |> 
    dplyr::select(
      `Alignment ID` = alignment_id,
      `Feature name` = feature_name,
      `Average Mz` = mz,
      `Average Rt(min)` = rt,
    )
  # Prepare the concentration table
  conc_mat <- sumexp[["conc"]]
  conc_df <- conc_mat |>
    round(.find_rounding_decimal_places(conc_mat)) |>    # Reduce the number of decimals
    tibble::as_tibble()
  # Original sample ID
  colnames(conc_df) <- SumExp::col_df(sumexp)$sample_name
  conc_df <- dplyr::bind_cols(feature_columns, conc_df)
  readr::write_tsv(conc_df, file, append = TRUE, col_names = TRUE, na = "")
}
