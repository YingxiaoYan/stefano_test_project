# ------------------------------------------------------------------------------------------- #
# Read a parsed MS-Dial file, preprocess, and save the data to an `rds` file
#
# During preprocessing for quality control, the intermediate data is saved to an `rds` file
# using the `proc$initialize_qc_steps` and `proc$append_to_qc_steps` functions. 
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  projlib/proc,       # Preprocessing functions
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir", "concentration_unit")

# Output files
FILE <- rlang::list2(
  # Processed data
  proc = msdial$get_raw_data_file_name(user_inputs, suffix = "proc"),
  # Intermediate status of the data
  qc = msdial$get_raw_data_file_name(user_inputs, suffix = "qc_steps"),
)
# To store intermediate data during quality control steps. `append_to_qc_steps` will append
proc$initialize_qc_steps(FILE$qc)
append_to_qc_steps <- function(...) proc$append_to_qc_steps(..., file = FILE$qc)

# Read the parsed data by `read-msdial.R`. If not available, warn the user
overall_sumexp <- msdial$read_parsed_msdial_data(user_inputs)
mat_id_for_norm <- "raw"  # `raw` or `vol_norm`
cat("\nPreprocessing steps are started.\n")

# Normalization using internal standards -------------------------------------------------

# Volumetric normalization using volumetric internal standards (vIS)
is_vIS <- SumExp::row_df(overall_sumexp)$std_type == "vIS"
if (any(is_vIS)) {      # This is optional
  # Store intermediate data during quality control steps
  append_to_qc_steps("volumetric internal std. raw" = overall_sumexp[is_vIS, ])
  overall_sumexp <- proc$normalize_volumetric(overall_sumexp, is_vIS, "raw")
  mat_id_for_norm <- "vol_norm"  # `raw` or `vol_norm`
} 

# Extract internal standard features
internal_std_se <- local(
  envir = list(se = overall_sumexp),
  {
    se <- se[quote(std_type == "IS"), ]
    se[quote(order(rt)), ]    # Sort by average retention time
  }
)
# Store intermediate data during quality control steps
append_to_qc_steps("internal std. before norm" = internal_std_se)

## Outlier removal     ---------------
# Number of outlying internal standard features per sample
n_outlier <- proc$count_outliers_per_sample(internal_std_se, mat_id_for_norm, times = 3)
n_feature <- nrow(internal_std_se)
# Outlying samples
is_outlier <- n_outlier > (0.2 * n_feature)
append_to_qc_steps(
  "internal std. number of outliers per sample" = n_outlier,
  "internal std. outlying samples" = is_outlier, 
)
if (any(is_outlier)) {
  # Exclude the outlying samples
  stopifnot(identical(names(is_outlier), colnames(internal_std_se)))
  internal_std_se <- internal_std_se[, !is_outlier]
  stopifnot(identical(names(is_outlier), colnames(overall_sumexp)))
  overall_sumexp <- overall_sumexp[, !is_outlier]
}

## Failed internal standards     ---------------
num_zeros <- proc$count_zeros_per_feature(internal_std_se[[mat_id_for_norm]])
is_failed <- num_zeros > 0
append_to_qc_steps("internal std. failed IS" = is_failed)
# Remove failed internal standard features
internal_std_se <- internal_std_se[!is_failed, ]

cat("Outliers and failed internal standards are removed.\n")

## Normalize the data using closest internal standard features     ---------------
closest_istd <- proc$get_value_of_closest_istd(
  se = overall_sumexp, 
  istd_se = internal_std_se, 
  mat_id = mat_id_for_norm, 
  rt = "rt"
)
overall_sumexp[["closest_norm"]] <- (overall_sumexp[[mat_id_for_norm]] / closest_istd) |> 
  labelled::set_variable_labels("Closest RT normalized peak area")
cat("Closest internal standard normalization is done.\n")

## LOESS fit over RT normalization     ---------------
overall_rt_range <- range(SumExp::row_df(overall_sumexp)$rt)    # Fit for RT of all features
excl_cat <- c("Blank", "CalCurve")

loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1,
  mat_id = mat_id_for_norm,
  rt = "rt"
)
# Normalize the data by LOESS fit along RT
rt <- SumExp::row_df(overall_sumexp)$rt
raw <- overall_sumexp[[mat_id_for_norm]]
overall_sumexp[["loess_norm"]] <- sapply(
  colnames(overall_sumexp), function(sample_id) {
    norm_factor <- predict(loess_fit[[sample_id]], newdata = rt)
    exp(log(raw[, sample_id]) - norm_factor)     # Normalize in log scale
  }
) |> 
  labelled::set_variable_labels("LOESS normalized peak area")
cat("LOESS normalization is done.\n")

## Blank subtraction     ---------------
norm_mat_ids <- c("loess_norm", "closest_norm")
norm_blk_mat_ids <- paste0(norm_mat_ids, "_blk")
overall_sumexp_before_blank <- overall_sumexp
overall_sumexp <- proc$subtract_blank_sumexp(
  x_se = overall_sumexp,
  contr_cat == "Blank", 
  contr_cat == "CalCurve",
  mat_ids = norm_mat_ids, 
  out_mat_ids = norm_blk_mat_ids
)

append_to_qc_steps(
  "excluded categories in normalization" = excl_cat, 
  "LOESS fit" = loess_fit,
  "normalized matrix ids" = norm_mat_ids,
  "normalized blank subtracted matrix ids" = norm_blk_mat_ids,
  "normalized" = overall_sumexp_before_blank, 
  "normalized - blank" = overall_sumexp, 
)
cat("Blank subtraction is done.\n")

# Calibration using calcurve -------------------------------------------------------------

#' Mark the expected preprocessing has been completed
mark_completed <- function() {
  append_to_qc_steps("Preprocessing Completed" = TRUE)
  cat("The intermediate objects during preprocessing saved to:", FILE$qc, "\n")
}
# When the data is produced without any targets (no calibration points), skip calibration
if (SumExp::metadata(overall_sumexp)$is_non_target_mode) {
  warning("NO CALIBRATION under non-target mode.")
  mark_completed()
  proc$stop_quietly()
} 

# Limit to the samples to be calibrated (or quantified)
quant_se <- overall_sumexp[quote(std_type == "Quant"), ]
# Before excluding out-of-range calibration concentrations
calcurve_se0 <- quant_se[, quote(contr_cat == "CalCurve")]
stopifnot("Calibration curve samples are required." = nrow(calcurve_se0) > 0)

# Per normalization method
per_norm_lst <- list()       # Collect the output
mat_ids_for_calib <- setNames(nm = norm_blk_mat_ids)
for(mat_id in norm_blk_mat_ids) {    # Use normalized and blank subtracted
  # Collect intermediate data during calibration.
  # `append_to_qc_steps` takes too long to save
  interm_data <- list()
  
  # Data is divided into two parts.
  # `calcurve_se` : calibration curve samples
  # `concn_se` : samples to be calibrated
  
  calcurve_se <- calcurve_se0     # Keep the unmodified for the next normalized data
  # Non-calcurve. `conc` will be added.
  concn_se <- quant_se[, quote(! contr_cat %in% "CalCurve")]
  stopifnot(identical(rownames(calcurve_se), rownames(concn_se)))  # Identical features
  
  # Meaningful min and max calibration points
  calcurve_se <- proc$add_calibration_curve_limits(calcurve_se, concn_se, mat_id)
  limit_df <- proc$extract_calibration_limit_pts(calcurve_se)
  SumExp::row_df(concn_se) <- cbind(SumExp::row_df(concn_se), limit_df)   # Copy limits
  
  # Exclude the features having no appropriate concentration range
  has_proper_range <- proc$has_proper_calibration_range(calcurve_se)
  interm_data[["calcurve conc ranges"]] <- cbind(limit_df, has_proper_range)
  calcurve_se <- calcurve_se[has_proper_range, ]
  concn_se    <-    concn_se[has_proper_range, ]
  interm_data[["calcurve_se all range"]] <- calcurve_se
  
  # Extract the signals of the lowest non-zero concentration points before replacing with NA
  lst_s <- proc$get_signals_of_calibration_nonzero_pts(calcurve_se, mat_id)
  llodq <- tibble::tibble(
    feature_id = names(lst_s),
    v = lst_s
  )
  
  # Replace the values outside the concentration range with NA
  calcurve_se <- proc$replace_outside_concentration_range_with_na(calcurve_se, mat_id)
  interm_data[["calcurve_se within range"]] <- calcurve_se
 
  # Fit the calibration curve
  cc_mat_norm <- calcurve_se[[mat_id]]
  c_concs <- proc$get_calcurve_concentrations(calcurve_se)
  calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
    proc$fit_and_test_calcurve_model(c_concs, cc_mat_norm[ii, ], penalty_quadratic = 0.01)
  })
  interm_data[["calcurve_models"]] <- calcurve_models
 
  # Find the LLOQ and LOD
  llodq <- llodq |>
    dplyr::rowwise() |>
    dplyr::mutate(
      lod_signal = proc$compute_llox_signal(v, 3), 
      lloq_signal = proc$compute_llox_signal(v, 10),
      lod = calcurve_models[[feature_id]]$best_model(lod_signal),
      lloq = calcurve_models[[feature_id]]$best_model(lloq_signal),
      lod = ifelse(lod < 0, 0, lod),
      lloq = ifelse(lloq < 0, 0, lloq)
    ) |> 
    dplyr::select(-v) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(dplyr::across(c(lod, lloq), ~ round(.x, 3)))
  SumExp::row_df(concn_se) <- cbind(SumExp::row_df(concn_se), llodq)

  # Compute the concentration of the samples using the calibration curve
  concn_se[["conc"]] <- proc$compute_concentration(
    concn_se, calcurve_se, calcurve_models, mat_id
  ) |> 
    labelled::set_label_attribute(
      paste0("Concentration [", user_inputs$concentration_unit, "]")
    ) |> 
    proc$replace_below_lloq_llod(llodq)
  # Label for the normalized data. To label the output of this function
  norm_lab <- labelled::get_label_attribute(quant_se[[mat_id]])
  labelled::label_attribute(concn_se) <- norm_lab
  # Maximum/minimum concentration after trimming out of each feature
  interm_data[["concn_se with conc"]] <- concn_se
  
  # Exclude the features with no quantification
  non_qc_conc <- concn_se[, quote(! contr_cat %in% "QC")][["conc"]]
  any_above_lloq <- non_qc_conc > llodq$lloq
  concn_se <- concn_se[rowSums(any_above_lloq) > 0, ]
  
  # Store the intermediate data in a file
  do.call("append_to_qc_steps", 
          setNames(list(interm_data), paste0("calibration/", mat_id)))
  # Collect the output
  per_norm_lst[[mat_id]] <- concn_se
  # Progress message
  cat("Calibration of", norm_lab, "data is done.\n")
}
# Save the processed data
saveRDS(per_norm_lst, file = FILE$proc)
cat("The `SumExp` object after preprocessing saved to:", FILE$proc, "\n")

# Mark the preprocessing step as completed
mark_completed()
