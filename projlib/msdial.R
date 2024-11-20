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
#' @return A tibble with sample information
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
  sinfo <- t(sample_lines[, indices, drop = FALSE])
  colnames(sinfo) <- header          # There was no header in `sinfo` before
  stopifnot(
    "Deviation from the expected names for samples" =
      identical(
        colnames(sinfo)[-5], 
        c("Class", "File type", "Injection order", "Batch ID")
      )
  )
  colnames(sinfo)[5] <- "given_sample_id"
  # Create a tibble with the sample information
  sinfo <- tibble::as_tibble(sinfo) |> 
    dplyr::select(
      Class,
      sample_type = "File type",
      injection_order = "Injection order",
      given_sample_id,
    ) |> 
    # Syntactically valid for variable name in R
    dplyr::mutate(sample_id = make.names(given_sample_id, unique = TRUE), .before = 1L)
  return(sinfo)
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
  # Read lines that contain chemical data
  readr::read_tsv(
    msdial_file, 
    skip = 4L,      # Data starts from the 5th line
    col_types = paste0(coltypes, collapse = ""),
    name_repair = "unique_quiet"
  )
}


#' Get the file name of the parsed data
#'
#' @param params A list of parameters including `input_file` and `intermediate_dir`.
#'
#' @return A text string of the file name
#' @export
get_sumexp_file_name <- function(params) {
  stopifnot(    # Required parameters
    !is.null(params$input_file), 
    !is.null(params$intermediate_dir)
  )
  file <- params$input_file
  dir <- params$intermediate_dir
  # Check if the user has write permission to the directory
  stopifnot(
    "The directory `params$intermediate_dir` should exists" = dir.exists(dir), 
    "Write permission is required to `params$intermediate_dir`" = file.access(dir, 2L) == 0
  )
  # Expected output file name from `read-msdial.R`
  file.path(dir, basename(file)) |> 
    tools::file_path_sans_ext(compression = TRUE) |> 
    paste0(".rds")
}

#' Read the parsed MS-DIAL data
#'
#' @param params A list of parameters including `input_file` and `intermediate_dir`.
#' @param r_script Path to the R script that parses the MS-DIAL output file, `read-msdial.R`
#'
#' @return A SummarizedExperiment object
#' @export
read_parsed_msdial_data <- function(params, r_script = "code/scripts/read-msdial.R") {
  stopifnot(!is.null(params$input_file))   # Required parameter
  sumexp_file <- get_sumexp_file_name(params)
  has_run_r_script <- FALSE
  if (! file.exists(sumexp_file)) {
    utils::capture.output(source(r_script))     # Print suppressed
    has_run_r_script <- TRUE
  }
  # Load the parsed data
  sumexp <- readRDS(sumexp_file)
  # Confirm the input file is the same as the one used in the parsing
  m5_f <- digest::digest(params$input_file, algo = "md5", file = TRUE)
  m5_se <- S4Vectors::metadata(sumexp)$file_md5    # Saved by `read-msdial.R`
  if (m5_f != m5_se) {
    if (has_run_r_script) {
      stop("The R script,", r_script, ", doesn't create properly")
    }
    utils::capture.output(source(r_script))     # Print suppressed
    sumexp <- readRDS(sumexp_file)
  }
  return(sumexp)
}


#' Write the LOESS normalized data
#'
#' @param sumexp A SummarizedExperiment object
#' @param assay The name of the assay to be written
#' @param in_file Path to the MS-DIAL output file that was used to create `sumexp`
#' @param out_file Path to the output tab-separated file
#' @export
write_normalized_data_tsv <- function(sumexp, assay, in_file, out_file) {
  # Prepare the chemical table
  i_sec <- get_three_section_indices(in_file)
  intact_chem_cols <- fetch_data_of_columns(in_file, i_sec[[1]])
  stopifnot(
    intact_chem_cols$"Alignment ID" == SummarizedExperiment::rowData(sumexp)$alignment_id
  )
  # Prepare the data table
  df_x <- SummarizedExperiment::assay(sumexp, assay) |> 
    round(0) |>     # Reduce the number of decimals
    tibble::as_tibble()
  # Original sample ID
  colnames(df_x) <- SummarizedExperiment::colData(sumexp)$given_sample_id
  df_x <- dplyr::bind_cols(intact_chem_cols, df_x)
  readr::write_tsv(df_x, file = out_file)
}

#' Extract QC samples into list
#'
#' @param se A SummarizedExperiment object
#'
#' @return A list of SummarizedExperiment objects divided by QC Classes
#' @export
extract_qc_samples_to_list <- function(se) {
  se <- se[, se$proc_cat == "QC"]
  qc_id <- se$Class
  # Column-wise split
  lapply(stats::setNames(nm = unique(qc_id)), \(ii) se[, qc_id == ii])
}

#' Get the internal standard chemicals from a SummarizedExperiment object
#'
#' @param se A SummarizedExperiment object
#'
#' @return A SummarizedExperiment object with the internal standard chemicals
#' @export
get_internal_std_se <- function(se) {
  # Extract the internal standard chemicals
  se <- se[SummarizedExperiment::rowData(se)$std_type == "IS", ]
  # Sort by average retention time
  se <- se[order(SummarizedExperiment::rowData(se)$rt), ]
  return(se)
}

