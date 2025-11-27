# ------------------------------------------------------------------------------------------- #
# Functions to read and write MS-DIAL raw/processed data
# ------------------------------------------------------------------------------------------- #

box::use(io = ./check_io_exist)

#' @export
box::use(
  ./show[tbl_chemical_summary],              # Export chemical summary table
)

# Project Parameters ---------------------------------------------------------------------

#' Find the `userin` has the required inputs
#'
#' @param userin A list of user input 
#' @param ... The required parameters in a character vector
#'
#' @md
#' @returns TRUE if all required inputs are provided
.stop_unless_has_required_inputs <- function(userin, ...) {
  for (p in c(...)) {
    if (is.null(userin[[p]])) stop(paste(p, "is required, but not provided."))
  } 
  invisible(TRUE)
}

#' Get the required input from the user
#'
#' @param ... The required parameters in a character vector
#' @returns A list of user input
#' @export
get_user_input <- function(...) {
  userin <- yaml::read_yaml("params.yml")
  # cat("User input has been read from 'params.yml'.\n")
  .stop_unless_has_required_inputs(userin, ...)
  userin
}

#' Get the file name of the parsed data
#' 
#' The file name has the same name as the `input_file` in the `user_inputs` with a `suffix`
#' and `.rds` added to it on the directory `intermediate_dir`.
#'
#' @param user_inputs A list of user inputs including `input_file` and `intermediate_dir`.
#' @param suffix A suffix to add to the file name to distinguish between data at different
#'   stages, such as raw and processed data.
#'
#' @returns A text string of the file name
#' @md
#' @export
get_raw_data_file_name <- function(user_inputs, suffix = "") {
  .stop_unless_has_required_inputs(user_inputs, "input_file", "intermediate_dir")
  file <- user_inputs$input_file
  dir <- io$mkdir_if_not_exist(user_inputs$intermediate_dir)
  # Expected output file name from `read-msdial.R`
  path <- file.path(dir, basename(file)) |> 
    tools::file_path_sans_ext(compression = TRUE)
  paste0(path, suffix, ".rds")
}

#' Read the parsed MS-DIAL data
#'
#' @param user_inputs A list of user inputs including `input_file` and `intermediate_dir`.
#'
#' @returns A [`SumExp::SumExp`] object
#' @md
#' @export
read_parsed_msdial_data <- function(user_inputs) {
  sumexp_file <- get_raw_data_file_name(user_inputs, suffix = "")
  stopifnot("Run `read-msdial.R first" = file.exists(sumexp_file))
  # Load the parsed data
  sumexp <- readRDS(sumexp_file)
  # Confirm the input file is the same as the one used in the parsing
  m5_f <- digest::digest(user_inputs$input_file, algo = "md5", file = TRUE)
  m5_se <- SumExp::metadata(sumexp)$file_md5    # Saved by `read-msdial.R`
  if (m5_f != m5_se) {
    stop("The input file is different from the one used in the parsing.",
         "Please re-run `read-msdial.R` first.")
  }
  sumexp
}


# Read MS-DIAL Files ---------------------------------------------------------------------

#' Identify three column sections in a MS-DIAL output file
#' 
#' MS-DIAL output files have three sections in columns. Information about "features",
#' "samples", and "group-wise summary" are stored in these sections. The 2nd section is
#' identified by the columns that have no NA values in the second row.
#' 
#' @param msdial_file Path to the MS-DIAL output file
#' 
#' @returns A list of three column indices
#' @export
get_three_section_indices <- function(msdial_file) {
  # Read the second (`File type`) line to identify three sections
  file_type_line <- utils::read.delim(
    msdial_file, header = FALSE, nrow = 5L, na.strings = c("NA", "")
  )[2, ]
  i_2nd <- range(which(! is.na(file_type_line)))    # Not NA is the 2nd column sections
  stopifnot(file_type_line[i_2nd[1L]] == "File type")    # As expected
  list(
    "1st" = 1L:i_2nd[1L],
    "2nd" = (i_2nd[1L] + 1L):i_2nd[2L],         # "+ 1L" for the title column of the rows
    "3rd" = (i_2nd[2L] + 1L):length(file_type_line)
  )
}

#' Fetch sample information from the first 5 lines
#'
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the columns that contain sample information. If it not
#'   provided, the 2nd section identified by `get_three_section_indices` will be used.
#' 
#' @returns A data frame with sample information
#' @md
#' @export
fetch_sample_info <- function(msdial_file, indices) {
  if (missing(indices)) indices <- get_three_section_indices(msdial_file)[["2nd"]]
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
    t() |>     # It is provided in rows
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
  # Syntactically valid names in R of sample names as `sample_id`s in row names
  rownames(sinfo) <- make.names(sinfo$sample_name, unique = TRUE)
  sinfo <- sinfo |> 
    dplyr::select(   # Syntactically valid column names in R
      Class,
      sample_type = "File type",
      injection_order = "Injection order",
      batch_id = "Batch ID",
      sample_name,
    )
  # For plots and tables
  sinfo <- sinfo |> 
    labelled::set_variable_labels(  
      Class = "Class",
      sample_type = "Sample Type",
      injection_order = "Injection Order",
      batch_id = "Batch ID",
      sample_name = "Sample ID",
    )
  sinfo
}

#' Fetch data of the selected columns of a MS-DIAL output file
#' 
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the selected columns
#' @param skip The number of lines to skip before reading the data. The default is 4. The data
#'   in MS_DIAL files starts from the 5th line.
#' 
#' @returns A tibble 
#' @seealso `get_three_section_indices` `fetch_sample_info`
#' @export
fetch_data_of_columns <- function(msdial_file, indices, skip = 4L) {
  stopifnot(!missing(indices))
  # Count total number of columns in the file
  l1 <- readr::read_tsv(
    msdial_file,
    skip = skip,
    n_max = 1L,
    col_names = FALSE,
    col_types = "c"
  )
  coltypes <- rep("-", ncol(l1))      # Skip the rest columns
  coltypes[indices] <- "c"
  # Read lines of the selected columns
  readr::read_tsv(
    msdial_file, 
    skip = skip,
    col_types = paste0(coltypes, collapse = ""),
    name_repair = "unique_quiet"
  )
}


# Export Data ----------------------------------------------------------------------------

#' Find the number of decimal places for rounding
#' 
#' The number of decimal places is determined by the median of the log10 of the absolute
#' values. The maximum number of decimal places is 4.
.find_rounding_decimal_places <- function(x) {
  x <- abs(x)
  x[x == 0] <- NA
  logx <- log10(x)
  m <- stats::median(logx, na.rm = TRUE)
  max(0, -floor(m) + 4)
}

#' Get the first three rows for sample information
.get_sample_info_rows <- function(sumexp, n_empty_cols) {
  s_rows <- SumExp::col_df(sumexp) |> 
    dplyr::select(
      "Class" = "Class",
      "Sample Type" = "sample_type",
      "Injection Order" = "injection_order",
      "Batch ID" = "batch_id",
    )
  if ("org_batch_id" %in% names(SumExp::col_df(sumexp))) {
    s_rows <- s_rows |> 
      dplyr::mutate(
        "Given Batch ID" = SumExp::col_df(sumexp)[["org_batch_id"]],
      )
  }
  # In rows
  out <- data.frame(
    matrix(data = NA, nrow = ncol(s_rows), ncol = n_empty_cols),    # Feature columns
    t(s_rows)
  )
  out[n_empty_cols] <- colnames(s_rows)
  out
}

#' Append exporting table with feature table
.append_export_table_with_feature_table <- function(sumexp, mat_id, feat_tbl) {
  mat_x <- sumexp[[mat_id]]
  df_x <- mat_x |>  
    round(.find_rounding_decimal_places(mat_x)) |>     # Reduce the number of decimals
    tibble::as_tibble()
  # Original sample ID
  colnames(df_x) <- SumExp::col_df(sumexp)$sample_name
  df_x |> 
    dplyr::mutate("Alignment ID" = SumExp::row_df(sumexp)$alignment_id, .before = 1) |> 
    dplyr::right_join(feat_tbl, y = _, by = "Alignment ID")
}

#' Merge sample info rows and feature data
.merge_sample_info_and_feature_data <- function(sumexp, mat_id, feat_tbl) {
  # The first three rows are the sample information
  sinfo <- .get_sample_info_rows(sumexp, n_empty_cols = ncol(feat_tbl))
  # Prepare the data table
  df_x <- .append_export_table_with_feature_table(sumexp, mat_id, feat_tbl)
  # Combine sample info rows and feature data
  col_nms <- colnames(sinfo) <- colnames(df_x)   # Avoid mismatch error
  col_nms_df <- as.data.frame(as.list(col_nms))  # Header row inbetween
  colnames(col_nms_df) <- col_nms
  out <- rbind(sinfo, col_nms_df, df_x)
  out
}

#' Export data with the feature table to a Excel `xlsx` file
#'
#' @param sumexp_lst A [ㅏSumExp::SumExp`] object or a list of such objects.
#'   Each object in the list corresponds to a different batch
#' @param mat_id The name of the data in `sumexp` (or assay) to be exported
#' @param in_file Path to the MS-DIAL output file that has been used to create `sumexp`.
#'   Unsaved feature information will be copied from this file.
#' @param out_file Path to the output file
#' @md
#' @export
export_data_with_feature_table_xlsx <- function(sumexp_lst, mat_id, in_file, out_file) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  # Prepare the feature table
  i_sec <- get_three_section_indices(in_file)
  intact_feature_cols <- fetch_data_of_columns(in_file, i_sec[[1]])
  
  # Prepare the data table
  if (inherits(sumexp_lst, "SumExp")) {
    .merge_sample_info_and_feature_data(sumexp_lst, mat_id, intact_feature_cols) |>
      writexl::write_xlsx(path = out_file, format_headers = FALSE, col_names = FALSE)
  } else {  # list of SumExp
    lst_df <- lapply(sumexp_lst, .merge_sample_info_and_feature_data, mat_id, intact_feature_cols)
    names(lst_df) <- paste("Batch", names(lst_df))
    writexl::write_xlsx(lst_df, path = out_file, format_headers = FALSE, col_names = FALSE)
  }
}

#' One summary feature table across batches
.summary_feature_table <- function(sumexp_lst) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  # Summary table for each batch
  chem_col_lst <- lapply(sumexp_lst, \(se) {
    tbl_chemical_summary(se) |> 
      dplyr::mutate(
        unit = SumExp::metadata(se)$concentration_unit,  # Add the unit
      )
  })
  # Summary table for all batches
  dplyr::bind_rows(chem_col_lst, .id = "Batch") |>
    dplyr::summarise(
      alignment_id = alignment_id[1L],
      chem_name = chem_name[1L],
      mz = mz[1L],
      .rt = .rt[1L],
      unit = unit[1L],
      dplyr::across(c(n_det, n_samples), ~ sum(.x, na.rm = TRUE)),
      model_r2 = mean(model_r2, na.rm = TRUE),
      .by = alignment_id
    ) |>
    dplyr::mutate(
      perc_detf = round(100 * n_det / n_samples, 1),
      n_d_s = paste0("(", n_det, "/", n_samples, ")"),
      model_r2 = round(model_r2, 3),
    ) |>  # Summary table
    dplyr::select(
      "Alignment ID" = alignment_id,
      "Chemical" = chem_name,
      "Average Mz" = mz,
      "Average Rt(min)" = .rt,
      "DF%" = perc_detf,
      "Samples (d/n)" = n_d_s,
      "Concentration" = unit,
      "Average R2" = model_r2,
    )
}

#' merge SumExp objects with identical features (rows)
.merge_sumexp_objs_with_identical_features <- function(sumexp_lst) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  for (ii in seq_along(sumexp_lst)[-1L]) {
    if (! identical(rownames(sumexp_lst[[1L]]), rownames(sumexp_lst[[ii]]))) {
      stop("All `SumExp` objects in `sumexp_lst` must have the same features (rows).",
           "Mismatch found in the ", ii, "-th element.")
    }
  }
  # Prepare
  sumexp_lst <- lapply(sumexp_lst, function(se) {
    # Required by `.append_export_table_with_feature_table`
    SumExp::row_df(se) <- SumExp::row_df(se)[, c("alignment_id"), drop = FALSE]
    methods::validObject(se)
    se
  })
  Reduce(cbind, sumexp_lst)
}


#' Export the concentration table
#'
#' @param sumexp_lst A list of [`SumExp::SumExp`] objects to be exported. The name of each element
#'   should be the batch ID.
#' @param file Path to the output file
#' @md
#' @export
export_concentration_xlsx <- function(sumexp_lst, file) {
  stopifnot(is.list(sumexp_lst))
  for (ii in seq_along(sumexp_lst)) {
    se <- sumexp_lst[[ii]]
    if (! inherits(se, "SumExp")) {
      stop("Each element of `sumexp_lst` must be a `SumExp` object.",
           "The ", ii, "-th element is a ", class(se), ".")
    }
  }
  # Leave only the features to export in any batch
  in_any <- sapply(sumexp_lst, \(se) SumExp::row_df(se)$to_export)
  in_any <- rowSums(in_any) > 0L
  sumexp_lst <- lapply(sumexp_lst, \(se) se[in_any, ])
  
  # Talbe of all batches
  # Overall summary table
  sum_all_batches <- .summary_feature_table(sumexp_lst)
  # SumExp object with all batches
  merged_se <- .merge_sumexp_objs_with_identical_features(sumexp_lst)
  # table to export including all batches
  all_batches_to_export_table <- .merge_sample_info_and_feature_data(
    merged_se,
    mat_id = "conc",
    feat_tbl = sum_all_batches
  )
  # Making table of pre-imputation conc of all batches
  all_batches_to_export_table_orig <- .merge_sample_info_and_feature_data(
    merged_se,
    mat_id = "conc_original",
    feat_tbl = sum_all_batches
  )
  
  # Per-batch tables to export
  per_batch_to_export_table <- lapply(sumexp_lst, \(se) {
    # Drop all features that are not to be exported per batch
    se <- se[SumExp::row_df(se)$to_export, ]
    # Prepare the chemical summary table per batch
    chem_col <- tbl_chemical_summary(se) |> 
      dplyr::mutate(
        unit = SumExp::metadata(se)$concentration_unit,  # Add the unit
      ) |>
      dplyr::mutate(      # Tidy up the table
        perc_detf = round(perc_detf, 1),
        n_d_s = paste0("(", n_det, "/", n_samples, ")"),
        model_r2 = round(model_r2, 3),
        dplyr::across(c(min, max, mean), ~ round(.x, 4)),
        dplyr::across(c(lod, lloq, min, max, mean), ~ as.character(.x)),
      ) |> 
      dplyr::select(
        "Alignment ID" = alignment_id,
        "Chemical" = chem_name,
        "Average Mz" = mz,
        "Average Rt(min)" = .rt,
        "DF%" = perc_detf,
        "Samples (d/n)" = n_d_s,
        "Concentration" = unit,
        "LOD" = lod,
        "LLOQ" = lloq,
        "Min Conc." = min,
        "Max Conc." = max,
        "Avg. Conc." = mean,
        "R2" = model_r2,
        "Model" = best_model,
        "Equation" = eqn,
        "N of points" = n_conc,
      )
    .merge_sample_info_and_feature_data(se, "conc", chem_col)
  })
  
  #Per batch original conc table
  per_batch_to_export_table_orig <- lapply(sumexp_lst, \(se) {
    # Drop all features that are not to be exported per batch
    se <- se[SumExp::row_df(se)$to_export, ]
    # Prepare the chemical summary table per batch
    chem_col <- tbl_chemical_summary(se) |> 
      dplyr::mutate(
        unit = SumExp::metadata(se)$concentration_unit,  # Add the unit
      ) |>
      dplyr::mutate(      # Tidy up the table
        perc_detf = round(perc_detf, 1),
        n_d_s = paste0("(", n_det, "/", n_samples, ")"),
        model_r2 = round(model_r2, 3),
        dplyr::across(c(min, max, mean), ~ round(.x, 4)),
        dplyr::across(c(lod, lloq, min, max, mean), ~ as.character(.x)),
      ) |> 
      dplyr::select(
        "Alignment ID" = alignment_id,
        "Chemical" = chem_name,
        "Average Mz" = mz,
        "Average Rt(min)" = .rt,
        "DF%" = perc_detf,
        "Samples (d/n)" = n_d_s,
        "Concentration" = unit,
        "LOD" = lod,
        "LLOQ" = lloq,
        "Min Conc." = min,
        "Max Conc." = max,
        "Avg. Conc." = mean,
        "R2" = model_r2,
        "Model" = best_model,
        "Equation" = eqn,
        "N of points" = n_conc,
      )
    .merge_sample_info_and_feature_data(se, "conc_original", chem_col)
  })
  
  
  # It had batch ID only, e.g. "1", "all"
  names(per_batch_to_export_table) <- paste("Batch", names(sumexp_lst))
  
  # Setting up stylization for excel documents
  half_style <- openxlsx::createStyle(fgFill = "yellow")
  quarter_style <- openxlsx::createStyle(fgFill = "red")
  
  # Making a range from which to pick out actual data of final format
  row_range <- c(7:nrow(per_batch_to_export_table[[1L]]))
  col_range <- c(17:ncol(per_batch_to_export_table[[1L]]))

  # If only 1 batch / all batches evaluated together
  if (length(sumexp_lst) == 1L) {
    
    #Add openxlsx functionality
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb=wb,
                           sheetName=names(per_batch_to_export_table))
    openxlsx::writeData(wb,
                        sheet=1,
                        x = per_batch_to_export_table[[1L]],
                        colNames = F)
    
    # Making number exclusive data frame of conc after imputation
    vals <- per_batch_to_export_table[[1L]][row_range,
                                            col_range]
    vals <- as.data.frame(sapply(vals, as.numeric))
    
    # Making LLOQ vector
    LLOQs <- as.numeric(per_batch_to_export_table[[1L]]$LLOQ[row_range])
    
    # Creating empty differ-matrix to fill with which imputed and how much
    diff_mat <- matrix(ncol=ncol(vals), nrow = nrow(vals))
    diff_mat[is.na(diff_mat)] <- 0
    
    # Finding values that differ = have been imputed
    for(curr_row in seq_len(nrow(vals))){
      which_half = which(vals[curr_row,] ==  (LLOQs[curr_row]/2))
      which_quarter = which(vals[curr_row,] ==  (LLOQs[curr_row]/4))
      
      # Checking which are half or quarter
      if(length(which_half) > 0){
        diff_mat[curr_row, which_half] <- 0.5
      }
      if(length(which_quarter) > 0){
        diff_mat[curr_row, which_quarter] <- 0.25
      }
    }
    
    
    # Go through every cell and change color depending on whether value in 
    # diff_mat is 1 or something else
    for(curr_row in seq_len(nrow(vals))){
      for(curr_col in seq_len(ncol(vals))){
        
        if(diff_mat[curr_row, curr_col] == 0.5){
          openxlsx::addStyle(wb,
                             sheet=1,
                             half_style,
                             curr_row+6,
                             curr_col+16)
        } else if(diff_mat[curr_row, curr_col] == 0.25){
          openxlsx::addStyle(wb,
                             sheet=1,
                             quarter_style,
                             curr_row+6,
                             curr_col+16)
          
        }
      }
    }

    openxlsx::saveWorkbook(wb, file, overwrite = T)
  } else {  # Multiple batches
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb=wb,
                           sheetName="Full")
    openxlsx::writeData(wb,
                        sheet=1,
                        x = all_batches_to_export_table,
                        colNames = F)
    
    for(i in seq_len(length(per_batch_to_export_table))){
      openxlsx::addWorksheet(wb=wb,
                             sheetName=names(per_batch_to_export_table)[i])
      openxlsx::writeData(wb,
                          sheet=(i+1),
                          x = per_batch_to_export_table[[i]],
                          colNames = F)
      
      # Making number exclusive data frame of conc after imputation
      vals <- per_batch_to_export_table[[i]][row_range,
                                              col_range]
      vals <- as.data.frame(sapply(vals, as.numeric))
      
      # Making LLOQ vector
      LLOQs <- as.numeric(per_batch_to_export_table[[i]]$LLOQ[row_range])
      
      # Creating empty differ-matrix to fill with which imputed and how much
      diff_mat <- matrix(ncol=ncol(vals), nrow = nrow(vals))
      diff_mat[is.na(diff_mat)] <- 0
      
      # Finding values that differ = have been imputed
      for(curr_row in seq_len(nrow(vals))){
        which_half = which(vals[curr_row,] ==  (LLOQs[curr_row]/2))
        which_quarter = which(vals[curr_row,] ==  (LLOQs[curr_row]/4))
        
        # Checking which are half or quarter
        if(length(which_half) > 0){
          diff_mat[curr_row, which_half] <- 0.5
        }
        if(length(which_quarter) > 0){
          diff_mat[curr_row, which_quarter] <- 0.25
        }
        
        #Applying style to each column in the row based on LLOQ
        for(curr_col in seq_len(ncol(vals))){
          
          if(diff_mat[curr_row, curr_col] == 0.5){
            openxlsx::addStyle(wb,
                               sheet=(i+1),
                               half_style,
                               curr_row+6,
                               curr_col+16)
          } else if(diff_mat[curr_row, curr_col] == 0.25){
            openxlsx::addStyle(wb,
                               sheet=(i+1),
                               quarter_style,
                               curr_row+6,
                               curr_col+16)
            
          }
        }
      }
    }
    
    openxlsx::saveWorkbook(wb, file, overwrite = T)
    # c(list("Full" = all_batches_to_export_table), per_batch_to_export_table) |>
    #   writexl::write_xlsx(path = file, format_headers = FALSE, col_names = FALSE)
  }
}  
