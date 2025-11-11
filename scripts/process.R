# ------------------------------------------------------------------------------------------- #
# Read a parsed MS-Dial file, process, and save the data to an `rds` file
# During processing for quality control, the intermediate data is saved to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Handle command line options     ---------------
# The options are used to control the processing steps
option_list <- rlang::list2(
  optparse::make_option(
    c("--per_batch"), type = "logical", default = TRUE,
    help = "Process data per batch? [default: %default]"
  ),
  optparse::make_option(
    c("--keep_cal_points"), type = "logical", default = TRUE,
    help = "Keep all cal points despite sample range? [default: %default]"
  ),
  optparse::make_option(
    c("--rm_outlier"), type = "logical", default = TRUE,
    help = "Remove outlier samples? [default: %default]"
  ),
  optparse::make_option(
    c("--log_calibration"), type = "logical", default = FALSE,
    help = "Use log scale for calibration curve fitting? [default: %default]"
  ),
  optparse::make_option(
    c("--weight"), type = "character", default = "largestR2",
    help = paste(
      "Weighting method for calibration curve fitting.",
      "Possible values are: `largestR2`, `1`, `1_div_x`, `1_div_x2`.",
      "[default: %default]"
    )
  ),
  optparse::make_option(
    c("--llox_method"), type = "character", default = "pt_signal_mean_plus_sd",
    help = "Method to compute LOD/LLOQ"
  ),
  optparse::make_option(
    c("--use_rsd20"), type = "logical", default = TRUE,
    help = "Use RSD% 20% threshold for LLOQ? [default: %default]"
  )
)
opt_parser <- optparse::OptionParser(
  option_list = option_list,
  usage = "Usage: %prog [options]",
  description = "Process MS-Dial parsed data and save the results."
)
params <- optparse::parse_args(opt_parser)
cat(
  "\nProcessing parameters:\n",
  "  - Per batch processing:", params$per_batch, "\n",
  "  - Keep cal points: ", params$keep_cal_points, "\n",
  "  - Outlier removal:", params$rm_outlier, "\n",
  "  - Log scale for calibration:", params$log_calibration, "\n",
  "  - Weight method:", params$weight, "\n",
  "  - LOD/LLOQ method:", params$llox_method, "\n",
  "  - Use RSD% 20% for LLOQ:", params$use_rsd20, "\n"
)

# Libraries and files     ---------------
# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  util = projlib/msdial_utils,        # Utility functions for MS-Dial data
  projlib/proc,       # Processing functions
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir")

# Output files
FILE <- rlang::list2(
  # Processed data
  proc = msdial$get_raw_data_file_name(user_inputs, suffix = "proc"),
  # Intermediate status of the data for reporting
  to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
)
# For reports, store intermediate data during quality control steps.
to_report <- list("params" = params, "Completed" = FALSE)

cat("\nProcessing steps are started.\n")
# Read the parsed data by `read-msdial.R`. If not available, warn the user
# This 'proc_sumexp' is the main data object that will undergo processing.
proc_sumexp <- msdial$read_parsed_msdial_data(user_inputs)

# Normalization using internal standards -------------------------------------------------

# For any preprocessing steps between reading the data and normalization, keep the raw data as is
# Label the matrix before normalization as "before_norm"
proc_sumexp[["before_norm"]] <- proc_sumexp[["raw"]] |> 
  labelled::set_variable_labels("No normalization")
to_report[["before normalization"]] <- proc_sumexp

# Extract internal standard features
internal_std_se <- local(
  envir = list(se = proc_sumexp),
  {
    se <- se[util$is_internal_std(se), ]
    se[order(util$retention_time(se)), ]    # Sort by average retention time
  }
)
# For reports, store intermediate data during quality control steps.
to_report[["internal std. before qc"]] <- internal_std_se

## Failed internal standards     ---------------
num_zeros <- proc$count_zeros_per_feature(internal_std_se[["before_norm"]])
# Mark features with > 10% zeros
is_failed <- num_zeros > ncol(internal_std_se) * 0.1
to_report[["is failed internal std."]] <- is_failed
# Remove failed internal standard features
is_failed_in_proc_sumexp <- rownames(proc_sumexp) %in% rownames(internal_std_se)[is_failed]
proc_sumexp <- proc_sumexp[!is_failed_in_proc_sumexp, ]
internal_std_se <- internal_std_se[!is_failed, ]
# Impute remaining zeros with the mean of the same type
internal_std_se <- proc$impute_zeros_with_mean_of_same_type(internal_std_se, "before_norm")
cat("Failed internal standards (N = ", sum(is_failed), ") are removed.\n", sep = "")

## Outlier sample removal     ---------------

if (params$rm_outlier) {    # Controlled by the command line option
  # Number of outlying internal standard features per sample
  n_out <- proc$count_outliers_per_sample(internal_std_se, "before_norm", times = 3)
  n_internal_std <- nrow(internal_std_se)
  # Outlier samples: number of outlying features > 20% of the total number of features
  is_outlier <- n_out > (0.2 * n_internal_std)
  to_report[["number of outlier internal std. per sample"]] <- n_out
  to_report[["is outlier sample"]] <- is_outlier

  if (any(is_outlier)) {
    # Exclude the outlying samples
    stopifnot(identical(names(is_outlier), colnames(internal_std_se)))
    internal_std_se <- internal_std_se[, !is_outlier]
    stopifnot(identical(names(is_outlier), colnames(proc_sumexp)))
    proc_sumexp <- proc_sumexp[, !is_outlier]
    cat("Outlier samples (N = ", sum(is_outlier), ") are removed.\n", sep = "")
  } else {
    cat("No outlier samples are found.\n")
  }
}


## Normalize the data using closest internal standard features     ---------------
closest_istd <- proc$get_value_of_closest_istd(
  se = proc_sumexp, 
  istd_se = internal_std_se, 
  mat_id = "before_norm"
)
proc_sumexp[["closest_norm"]] <- (proc_sumexp[["before_norm"]] / closest_istd) |> 
  labelled::set_variable_labels("Closest RT normalized")
cat("Closest internal standard normalization is done.\n")

## LOESS fit over RT normalization     ---------------
overall_rt_range <- range(util$retention_time(proc_sumexp))    # Fit for RT of all features
excl_cat <- c("Blank", "CalCurve")
to_report[["excluded categories in normalization"]] <- excl_cat

loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1,
  mat_id = "before_norm"
)
to_report[["LOESS fit"]] <- loess_fit
# Normalize the data by LOESS fit along RT
rt <- util$retention_time(proc_sumexp)
proc_sumexp[["loess_norm"]] <- sapply(
  colnames(proc_sumexp), function(sample_id) {
    norm_factor <- predict(loess_fit[[sample_id]], newdata = rt)
    exp(log(proc_sumexp[["before_norm"]][, sample_id]) - norm_factor)     # Normalize in log scale
  }
) |> 
  labelled::set_variable_labels("LOESS normalized")
cat("LOESS normalization is done.\n")

## Blank subtraction     ---------------
norm_mat_ids <- c("loess_norm", "closest_norm", "before_norm")
to_report[["normalized matrix ids"]] <- norm_mat_ids
to_report[["normalized"]] <- proc_sumexp    # Before blank subtraction. For reports

# Get batch IDs
batch_ids <- as.character(SumExp::col_df(proc_sumexp)[["batch_id"]])
if (params$per_batch) {
  cat(
    "Processing blank substraction per batch.", 
    "Found batch_ids:", paste(unique(batch_ids), collapse = ", "), "\n"
  )
} else {
  # If not processing per batch, assign all samples to a single batch
  SumExp::col_df(proc_sumexp)[["org_batch_id"]] <- batch_ids   # Keep the original batch IDs
  batch_ids <- rep("all", ncol(proc_sumexp))
  SumExp::col_df(proc_sumexp)[["batch_id"]] <- batch_ids
}
# Split the data by batch IDs
per_batch_proc_se_lst <- SumExp::split_columns(proc_sumexp, batch_ids)

norm_blk_mat_ids <- util$mat_id_of_blank_subtracted(norm_mat_ids)
# Per batch, then per normalization method
for (batch_id in unique(batch_ids)) {
  se <- per_batch_proc_se_lst[[batch_id]]
  per_batch_proc_se_lst[[batch_id]] <- proc$add_blank_subtracted_sumexp(
    sumexp = se,
    no_change = util$ctrl_smpl_cat(se) == "CalCurve",
    mat_ids = norm_mat_ids, 
    out_mat_ids = norm_blk_mat_ids
  )
}
to_report[["normalized - blank"]] <- per_batch_proc_se_lst
cat("Blank subtraction is done.\n")

# Calibration using calcurve -------------------------------------------------------------

#' Mark the expected processing has been completed
mark_completed <- function() {
  cat("Processing steps are completed.\nSaving intermediate data during the processing...\n")
  to_report[["Completed"]] <- TRUE
  saveRDS(to_report, FILE$to_rep)
  cat("The intermediate data saved to:", FILE$to_rep, "\n")
}
# When the data is produced without any targets (no calibration points), skip calibration
if (SumExp::metadata(proc_sumexp)$is_non_target_mode) {
  warning("NO CALIBRATION under non-target mode.")
  mark_completed()
  proc$stop_quietly()
} 

log_scale <- params$log_calibration
if (log_scale) cat("Log scale for calibration curve fitting is applied.\n")

if (params$per_batch) cat("Processing calibration per batch.")
# Collect the output
lst_proc <- list()

# Per batch, then per normalization method
for (batch_id in unique(batch_ids)) {
  in_batch <- batch_ids == batch_id
  cat("Processing batch_id =", batch_id, "with", sum(in_batch), "samples.\n")
  lst_proc[[batch_id]] <- list()    # To collect the output per batch
  
  # Per normalization method
  for (mat_id0 in norm_blk_mat_ids) {    # Use normalized and blank subtracted
    quant_se <- per_batch_proc_se_lst[[batch_id]]   # Change across the steps
    # Limit to the samples to be calibrated (or quantified)
    # Before excluding out-of-range calibration concentrations
    quant_se <- quant_se[util$is_targeted_feature(quant_se), ]
    # Sort by chemical name
    quant_se <- quant_se[order(SumExp::row_df(quant_se)$feature_name), ]
    # Label for the normalized data.
    norm_lab <- labelled::get_label_attribute(quant_se[[mat_id0]])
   
    # Log scale for calibration curve fitting
    if (log_scale) quant_se[[mat_id0]] <- log1p(quant_se[[mat_id0]])    # log1p(x) = log(x + 1)
    
    # Meaningful min and max calibration points + LOD/LLOQ
    fun <- switch(
      params$llox_method,
      "pt_signal_mean" = proc$compute_llox_signal_using_mean_times,
      "pt_signal_mean_plus_sd" = proc$compute_llox_signal_using_mean_plus_sd_times
    )
    quant_se <- proc$find_calib_lim_pts_and_llox_from_llox_signal(
      quant_se, mat_id0, fun, params$use_rsd20
    )

    # Exclude the chemicals having no appropriate concentration range
    has_proper_range <- proc$has_proper_calibration_range(quant_se)
    SumExp::row_df(quant_se) <- cbind(SumExp::row_df(quant_se), has_proper_range)
    if (sum(has_proper_range) == 0) {     # No chemicals with proper range
      warning("NO Valid calibration points for any chemical. But reports can be generated.")
      lst_proc[[batch_id]][[mat_id0]] <- quant_se
      next
    }
    # Matrix ID for calibration
    mat_id <- util$mat_id_for_calibration(mat_id0)
    quant_se[[mat_id]] <- util$extract_with_na(quant_se[[mat_id0]], i = has_proper_range, j = TRUE)
    if (log_scale) quant_se[[mat_id0]] <- expm1(quant_se[[mat_id0]])    # Back transform
    
    # Data is divided into two parts.
    # `calcurve_se` : calibration curve samples
    # `concn_se` : samples to be calibrated
    lst_q_se <- util$split_into_calcurve_and_other(quant_se, c("cc", "concn"))
    calcurve_se <- lst_q_se$cc
    concn_se    <- lst_q_se$concn

    # Replace the values outside the concentration range with NA
    calcurve_se <- proc$replace_outside_concentration_range_with_na(calcurve_se, mat_id)
  
    # Fit the calibration curve
    cc_mat_norm <- calcurve_se[[mat_id]]
    c_concs <- util$spiked_conc_pts(calcurve_se)
    if (log_scale) {
      c_concs <- log(c_concs)  # `signal` is assumed to be log-transformed already
      if (params$weight != "1") {
        warning("Weighting methods other than `1` are not available under log scale. Using `1`.")
        params$weight <- "1"
      }
    }
    calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
      proc$fit_and_test_calcurve_model(
        c_concs,
        signal = cc_mat_norm[ii, ],
        weight_method = params$weight,
        penalty_quadratic = 0.01
      )
    })
    SumExp::row_df(concn_se) <- SumExp::row_df(concn_se) |> 
      dplyr::mutate(calcurve_model = calcurve_models)
    # Back-transform to the original scale 
    if (log_scale) calcurve_se[[mat_id]] <- expm1(calcurve_se[[mat_id]])    # expm1(x) = exp(x) - 1
    
    # Compute the concentration of the samples using the calibration curve
    concn_se[["conc0"]] <- proc$compute_concentration(concn_se, mat_id, log_scale = log_scale) |> 
      labelled::set_label_attribute("Concentration before filtering")
    concn_se <- concn_se |>
      proc$replace_below_lod_lloq(conc_mat_id = "conc0") |>
      proc$replace_conc_whose_signal_below_lloq(signal_mat_id = mat_id, conc_mat_id = "conc0") |>
      proc$replace_conc_whose_signal_above_lloq(signal_mat_id = mat_id, conc_mat_id = "conc0")
    labelled::label_attribute(concn_se) <- norm_lab
    if (log_scale) concn_se[[mat_id]] <- expm1(concn_se[[mat_id]])    # Back transform

    # Exclude the chemicals with no quantification
    non_qc_conc <- util$exclude_ctrl_smpl_cat(concn_se, "QC")[["conc0"]]
    any_above_lloq <- non_qc_conc > SumExp::row_df(concn_se)[, "lloq"]
    has_ex <- rowSums(any_above_lloq) > 0
    has_ex[is.na(has_ex)] <- FALSE
    SumExp::row_df(concn_se)[["to_export"]] <- has_ex
    unit <- SumExp::metadata(concn_se)$concentration_unit
    concn_se[["conc"]] <- util$extract_with_na(concn_se[["conc0"]], i = has_ex, j = TRUE) |> 
      labelled::set_label_attribute(paste0("Concentration [", unit, "]"))
    
    # Collect the output
    lst_proc[[batch_id]][[mat_id]] <- rlang::list2(
      calcurve = calcurve_se,
      concn = concn_se
    )
    # Progress message
    cat("Calibration of", norm_lab, "data is done.\n")
  } # End of per normalization method
}
# Save the processed data
saveRDS(lst_proc, file = FILE$proc)
cat("The `SumExp` object after processing saved to:", FILE$proc, "\n")

# Mark the processing step as completed
mark_completed()
