# ------------------------------------------------------------------------------------------- #
# Read a parsed MS-Dial file, preprocess, and save the data to an `rds` file
#
# During preprocessing for quality control, the intermediate data is saved to an `rds` file
# using the `proc$initialize_qc_steps` and `proc$append_to_qc_steps` functions. 
# ------------------------------------------------------------------------------------------- #

if (!exists("param_weight")) param_weight <- "lowestR2"   # Default weight method
cat("\nPreprocessing parameters:\n",
    "  - Weight method: ", param_weight, "\n")

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  util = projlib/msdial_utils,        # Utility functions for MS-Dial data
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
norm_blk_mat_ids <- proc$get_mat_id_of_blank_subtracted(norm_mat_ids)
overall_sumexp_before_blank <- overall_sumexp
overall_sumexp <- proc$add_blank_substracted_sumexp(
  x_se = overall_sumexp,
  no_change = util$ctrl_smpl_cat(overall_sumexp) == "CalCurve",
  mat_ids = norm_mat_ids, 
  out_mat_ids = norm_blk_mat_ids
)

append_to_qc_steps(
  "excluded categories in normalization" = excl_cat, 
  "LOESS fit" = loess_fit,
  "normalized matrix ids" = norm_mat_ids,
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
# Before excluding out-of-range calibration concentrations
quant_se0 <- overall_sumexp[quote(std_type == "Quant"), ]

# Per normalization method
per_norm_lst <- list()       # Collect the output
for(mat_id in norm_blk_mat_ids) {    # Use normalized and blank subtracted
  # Collect intermediate data during calibration.
  # `append_to_qc_steps` takes too long to save
  interm_data <- list()
  
  quant_se <- quant_se0     # Keep the unmodified for the next normalized data
  # Meaningful min and max calibration points
  quant_se <- proc$add_calibration_curve_limits(quant_se, mat_id)
  
  # Exclude the chemicals having no appropriate concentration range
  limit_df <- proc$extract_calibration_limit_pts(quant_se)
  has_proper_range <- proc$has_proper_calibration_range(quant_se)
  interm_data[["calcurve conc ranges"]] <- cbind(limit_df, has_proper_range)
  quant_se <- quant_se[has_proper_range, ]
  # Data is divided into two parts.
  # `calcurve_se` : calibration curve samples
  # `concn_se` : samples to be calibrated
  lst_q_se <- util$split_into_calcurve_and_other(quant_se, c("cc", "concn"))
  calcurve_se <- lst_q_se$cc
  concn_se    <- lst_q_se$concn
  interm_data[["calcurve_se all range"]] <- calcurve_se
  
  # Replace the values outside the concentration range with NA
  calcurve_se <- proc$replace_outside_concentration_range_with_na(calcurve_se, mat_id)
  interm_data[["calcurve_se within range"]] <- calcurve_se
 
  # Fit the calibration curve
  cc_mat_norm <- calcurve_se[[mat_id]]
  c_concs <- util$spiked_conc_pts(calcurve_se)
  calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
    proc$fit_and_test_calcurve_model(c_concs,
                                     signal = cc_mat_norm[ii, ],
                                     weight_method = param_weight,
                                     penalty_quadratic = 0.01)
  })
  interm_data[["weight method"]] <- param_weight
  
  # Find the LLOQ and LOD
  llodq <- proc$extract_calibration_limit_pts(calcurve_se)[, c("lod_signal", "lloq_signal")] |> 
    tibble::rownames_to_column("chem_id")        # Keep the IDs through dplyr::...
  stopifnot(identical(llodq$chem_id, names(calcurve_models)))
  llodq <- llodq |> 
    dplyr::mutate(calcurve_model = calcurve_models) |> 
    dplyr::rowwise() |>
    dplyr::mutate(
      lod = calcurve_model$best_model(lod_signal),
      lloq = calcurve_model$best_model(lloq_signal),
      lod = ifelse(lod < 0, 0, lod),
      lloq = ifelse(lloq < 0, 0, lloq)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::select(chem_id, calcurve_model, lod, lloq)
  # SumExp@row_df is a data frame with row names.
  llodq <- tibble::column_to_rownames(llodq, "chem_id")
  stopifnot(identical(rownames(llodq), rownames(concn_se)))
  SumExp::row_df(concn_se) <- cbind(SumExp::row_df(concn_se), llodq)

  # Compute the concentration of the samples using the calibration curve
  concn_se[["conc"]] <- proc$compute_concentration(concn_se, mat_id) |> 
    labelled::set_label_attribute(
      paste0("Concentration [", user_inputs$concentration_unit, "]")
    ) |> 
    proc$replace_below_lloq_llod(llodq)
  # Label for the normalized data. To label the output of this function
  norm_lab <- labelled::get_label_attribute(quant_se[[mat_id]])
  labelled::label_attribute(concn_se) <- norm_lab
  # Maximum/minimum concentration after trimming out of each chemical
  interm_data[["concn_se with conc"]] <- concn_se
  
  # Exclude the chemicals with no quantification
  non_qc_conc <- util$exclude_ctrl_smpl_cat(concn_se, "QC")[["conc"]]
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
