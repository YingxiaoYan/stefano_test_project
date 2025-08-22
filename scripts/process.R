# ------------------------------------------------------------------------------------------- #
# Read a parsed MS-Dial file, process, and save the data to an `rds` file
# During processing for quality control, the intermediate data is saved to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Handle command line options     ---------------
# The options are used to control the processing steps
option_list <- rlang::list2(
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
)
opt_parser <- optparse::OptionParser(
  option_list = option_list,
  usage = "Usage: %prog [options]",
  description = "Process MS-Dial parsed data and save the results."
)
params <- optparse::parse_args(opt_parser)
cat(
  "\nProcessing parameters:\n",
  "  - Outlier removal:", params$rm_outlier, "\n",
  "  - Log scale for calibration:", params$log_calibration, "\n",
  "  - Weight method:", params$weight, "\n",
  "  - LOD/LLOQ method:", params$llox_method, "\n"
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
  # Intermediate status of the data
  qc = msdial$get_raw_data_file_name(user_inputs, suffix = "qc_steps"),
)
# For reports, store intermediate data during quality control steps.
qc_steps <- list()

cat("\nProcessing steps are started.\n")
# Read the parsed data by `read-msdial.R`. If not available, warn the user
# This 'proc_sumexp' is the main data object that will undergo processing.
proc_sumexp <- msdial$read_parsed_msdial_data(user_inputs)

# Normalization using internal standards -------------------------------------------------

# Volumetric normalization using volumetric internal standards (vIS)
is_vIS <- util$std_type(proc_sumexp) == "vIS"
if (any(is_vIS)) {      # This is optional
  qc_steps[["is volumetric internal std."]] <- is_vIS    # For reports
  proc_sumexp <- proc$normalize_volumetric(proc_sumexp, is_vIS, "raw")
} 
# If no volumetric internal standards, use raw data for normalization that uses internal standards
mat_id <- if (any(is_vIS)) "vol_norm" else "raw"
# Label the matrix before normalization as "before_norm"
proc_sumexp[["before_norm"]] <- proc_sumexp[[mat_id]] |> 
  labelled::set_variable_labels("No normalization")

# Extract internal standard features
internal_std_se <- local(
  envir = list(se = proc_sumexp),
  {
    se <- se[util$is_internal_std(se), ]
    se[order(util$retention_time(se)), ]    # Sort by average retention time
  }
)
# For reports, store intermediate data during quality control steps.
qc_steps[["internal std. before qc"]] <- internal_std_se

## Failed internal standards     ---------------
num_zeros <- proc$count_zeros_per_feature(internal_std_se[["before_norm"]])
# Mark features with > 10% zeros
is_failed <- num_zeros > ncol(internal_std_se) * 0.1
qc_steps[["is failed internal std."]] <- is_failed    # For reports
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
  qc_steps[["number of outlier internal std. per sample"]] <- n_out    # For reports
  qc_steps[["is outlier sample"]] <- is_outlier    # For reports

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
qc_steps[["excluded categories in normalization"]] <- excl_cat    # For reports

loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1,
  mat_id = "before_norm"
)
qc_steps[["LOESS fit"]] <- loess_fit    # For reports
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
qc_steps[["normalized matrix ids"]] <- norm_mat_ids    # For reports
norm_blk_mat_ids <- util$mat_id_of_blank_subtracted(norm_mat_ids)
qc_steps[["normalized"]] <- proc_sumexp    # Before blank subtraction. For reports
proc_sumexp <- proc$add_blank_subtracted_sumexp(
  sumexp = proc_sumexp,
  no_change = util$ctrl_smpl_cat(proc_sumexp) == "CalCurve",
  mat_ids = norm_mat_ids, 
  out_mat_ids = norm_blk_mat_ids
)
qc_steps[["normalized - blank"]] <- proc_sumexp    # For reports
cat("Blank subtraction is done.\n")

# Calibration using calcurve -------------------------------------------------------------

#' Mark the expected processing has been completed
mark_completed <- function() {
  cat("Processing steps are completed.\nSaving intermediate data during the processing...\n")
  qc_steps[["Completed"]] <- TRUE
  saveRDS(qc_steps, FILE$qc)
  cat("The intermediate data saved to:", FILE$qc, "\n")
}
# When the data is produced without any targets (no calibration points), skip calibration
if (SumExp::metadata(proc_sumexp)$is_non_target_mode) {
  warning("NO CALIBRATION under non-target mode.")
  mark_completed()
  proc$stop_quietly()
} 

# Limit to the samples to be calibrated (or quantified)
# Before excluding out-of-range calibration concentrations
quant_se0 <- proc_sumexp[util$is_targeted_feature(proc_sumexp), ]

# Per normalization method
per_norm_lst <- list()       # Collect the output
for (mat_id in norm_blk_mat_ids) {    # Use normalized and blank subtracted
  # Collect intermediate data during calibration.
  interm_data <- list()
  
  quant_se <- quant_se0     # Keep the unmodified for the next normalized data
  if (params$log_calibration) {
    # Log scale for calibration curve fitting
    quant_se[[mat_id]] <- log1p(quant_se[[mat_id]])    # log1p(x) = log(x + 1)
    interm_data[["log scale"]] <- TRUE
    cat("Log scale for calibration curve fitting is applied.\n")
  } else {
    interm_data[["log scale"]] <- FALSE
  }

  # Meaningful min and max calibration points + LOD/LLOQ
  fun <- switch(
    params$llox_method,
    "pt_signal_mean" = proc$compute_llox_signal_using_mean_times,
    "pt_signal_mean_plus_sd" = proc$compute_llox_signal_using_mean_plus_sd_times
  )
  interm_data[["llox method"]] <- params$llox_method
  quant_se <- proc$find_calib_lim_pts_and_llox_from_llox_signal(quant_se, mat_id, fun)
  
  # Exclude the chemicals having no appropriate concentration range
  has_proper_range <- proc$has_proper_calibration_range(quant_se)
  interm_data[["calcurve_se incl failed"]] <- local({
    cc <- util$split_into_calcurve_and_other(quant_se)$CalCurve
    SumExp::row_df(cc) <- cbind(SumExp::row_df(cc), has_proper_range)
    cc
  })
  if (sum(has_proper_range) == 0) {     # No chemicals with proper range
    warning("NO Valid calibration points for any chemical. But reports can be generated.")
    qc_steps[[paste0("calibration/", mat_id)]] <- interm_data
    next
  }
  quant_se <- quant_se[has_proper_range, ]
  # Data is divided into two parts.
  # `calcurve_se` : calibration curve samples
  # `concn_se` : samples to be calibrated
  lst_q_se <- util$split_into_calcurve_and_other(quant_se, c("cc", "concn"))
  calcurve_se <- lst_q_se$cc
  concn_se    <- lst_q_se$concn
  # Replace the values outside the concentration range with NA
  calcurve_se <- proc$replace_outside_concentration_range_with_na(calcurve_se, mat_id)
  interm_data[["calcurve_se within range"]] <- calcurve_se
 
  # Fit the calibration curve
  cc_mat_norm <- calcurve_se[[mat_id]]
  c_concs <- util$spiked_conc_pts(calcurve_se)
  calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
    proc$fit_and_test_calcurve_model(c_concs,
                                     signal = cc_mat_norm[ii, ],
                                     weight_method = params$weight,
                                     penalty_quadratic = 0.01,
                                     log_scale = params$log_calibration)  # non-negative weights
  })
  stopifnot(identical(names(calcurve_models), rownames(concn_se)))
  SumExp::row_df(concn_se) <- SumExp::row_df(concn_se) |> 
    dplyr::mutate(calcurve_model = calcurve_models)
  interm_data[["weight method"]] <- params$weight
  
  # Compute the concentration of the samples using the calibration curve
  unit <- SumExp::metadata(concn_se)$concentration_unit
  concn_se[["conc"]] <- proc$compute_concentration(concn_se, mat_id) |> 
    labelled::set_label_attribute(
      paste0("Concentration [", unit, "]")
    )
  if (params$log_calibration) {
    concn_se[["conc"]] <- exp(concn_se[["conc"]])    # Return to original scale
  }
  concn_se <- concn_se |>
    proc$replace_below_lod_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_below_lloq(signal_mat_id = mat_id, conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_above_lloq(signal_mat_id = mat_id, conc_mat_id = "conc")
  # Label for the normalized data. To label the output of this function
  norm_lab <- labelled::get_label_attribute(quant_se[[mat_id]])
  labelled::label_attribute(concn_se) <- norm_lab
  # Maximum/minimum concentration after trimming out of each chemical
  interm_data[["concn_se with conc"]] <- concn_se
  
  # Exclude the chemicals with no quantification
  non_qc_conc <- util$exclude_ctrl_smpl_cat(concn_se, "QC")[["conc"]]
  any_above_lloq <- non_qc_conc > SumExp::row_df(concn_se)[, "lloq"]
  concn_se <- concn_se[rowSums(any_above_lloq) > 0, ]
  
  # Store the intermediate data
  qc_steps[[paste0("calibration/", mat_id)]] <- interm_data
  # Collect the output
  per_norm_lst[[mat_id]] <- concn_se
  # Progress message
  cat("Calibration of", norm_lab, "data is done.\n")
}
# Save the processed data
saveRDS(per_norm_lst, file = FILE$proc)
cat("The `SumExp` object after processing saved to:", FILE$proc, "\n")

# Mark the processing step as completed
mark_completed()
