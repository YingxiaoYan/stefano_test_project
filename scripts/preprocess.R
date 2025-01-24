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

# Read the parsed data by `read-msdial.R`. If not available, warn the user
overall_sumexp <- msdial$read_parsed_msdial_data(user_inputs)
mat_id_for_norm <- "raw"  # Feature intensity to be used for normalization
# Normalization using internal standards -------------------------------------------------

# Volumetric normalization using volumetric internal standards (vIS)
is_vIS <- SumExp::row_df(overall_sumexp)$std_type == "vIS"
if (any(is_vIS)) {
  stopifnot("Only one volumetric internal standard is allowed." = sum(is_vIS) == 1)
  vol_internal_std_se <- overall_sumexp[is_vIS, ]
  overall_sumexp <- overall_sumexp[!is_vIS, ]
  
  v <- as.vector(vol_internal_std_se[["raw"]])
  mat <- t(replicate(nrow(overall_sumexp), v))  # Column-wise normalization
  overall_sumexp[["vol_norm"]] <- overall_sumexp[["raw"]] / mat * mean(v, na.rm = TRUE)
  labelled::label_attribute(overall_sumexp[["vol_norm"]]) <- "Volumetric Normalized" 
  mat_id_for_norm <- "vol_norm"        # Feature intensity to be used for normalization
  # Store intermediate data during quality control steps
  proc$append_to_qc_steps("volumetric internal std. raw" = vol_internal_std_se, file = FILE$qc)
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
proc$append_to_qc_steps("internal std. before norm" = internal_std_se, file = FILE$qc)

# Outlier removal     ---------------
# Number of outlying internal standard features per sample
n_outlier <- proc$count_outliers_per_sample(internal_std_se, mat_id_for_norm, times = 3)
n_feature <- nrow(internal_std_se)
# Outlying samples
is_outlier <- n_outlier > (0.2 * n_feature)
proc$append_to_qc_steps(
  "internal std. number of outliers per sample" = n_outlier,
  "internal std. outlying samples" = is_outlier, 
  file = FILE$qc
)
if (any(is_outlier)) {
  # Exclude the outlying samples
  stopifnot(identical(names(is_outlier), colnames(internal_std_se)))
  internal_std_se <- internal_std_se[, !is_outlier]
  stopifnot(identical(names(is_outlier), colnames(overall_sumexp)))
  overall_sumexp <- overall_sumexp[, !is_outlier]
}

# Failed internal standards     ---------------
num_zeros <- proc$count_zeros_per_feature(internal_std_se[[mat_id_for_norm]])
is_failed <- num_zeros > 0
proc$append_to_qc_steps("internal std. failed IS" = is_failed, file = FILE$qc)
# Remove failed internal standard features
internal_std_se <- internal_std_se[!is_failed, ]

# Normalize the data using closest internal standard features     ---------------
closest_istd <- proc$get_value_of_closest_istd(
  se = overall_sumexp, 
  istd_se = internal_std_se, 
  mat_id = mat_id_for_norm, 
  rt = "rt"
)
overall_sumexp[["closest_norm"]] <- (overall_sumexp[[mat_id_for_norm]] / closest_istd) |> 
  labelled::set_variable_labels("Closest RT Normalized")

# LOESS fit over RT normalization     ---------------
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
  labelled::set_variable_labels("LOESS Normalized")

# Blank subtraction     ---------------
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

proc$append_to_qc_steps(
  "excluded categories in normalization" = excl_cat, 
  "LOESS fit" = loess_fit,
  "normalized matrix ids" = norm_mat_ids,
  "normalized blank subtracted matrix ids" = norm_blk_mat_ids,
  "normalized" = overall_sumexp_before_blank, 
  "normalized - blank" = overall_sumexp, 
  file = FILE$qc
)

# Calibration using calcurve -------------------------------------------------------------

if (SumExp::metadata(overall_sumexp)$is_non_target_mode) {
  warning("NO CALIBRATION under non-target mode.")
} else {
  # Extract quantification standard data
  quant_se <- overall_sumexp[quote(std_type == "Quant"), ]
  # Before excluding out-of-range calibration concentrations
  calcurve_se0 <- quant_se[, quote(contr_cat == "CalCurve")]
  stopifnot("Calibration curve samples are required." = nrow(calcurve_se0) > 0)
  # Calibration zero samples (Not modified during calibration)
  cal_0_se <- calcurve_se0[, quote(c_conc == 0)] 
  stopifnot("Multiple `Cal_0` samples are required." = ncol(cal_0_se) > 1)
  
  # Per normalization method
  per_norm_lst <- lapply(setNames(nm = norm_blk_mat_ids), \(mat_id) {
    # Store intermediate data during calibration. `append_to_qc_steps` takes too long to save
    interm_data <- list()
    
    calcurve_se <- calcurve_se0[, quote(c_conc != 0)]          # Non-zero conc
    # Non-calcurve. `conc` will be added.
    concn_se <- quant_se[, quote(! contr_cat %in% "CalCurve")]
    SumExp::col_df(concn_se)$c_conc <- NULL     # Non-calcurve doesn't have known concentration
    stopifnot(identical(rownames(calcurve_se), rownames(concn_se)))  # Identical features
    
    # Label for the normalized data. To label the output of this function
    norm_lab <- labelled::get_label_attribute(quant_se[[mat_id]]) |> 
      stringr::str_replace("Normalized", "normalization")
    
    c_concs <- SumExp::col_df(calcurve_se)$c_conc   # Available concentrations
    limit_df <- proc$identify_limts_in_calibrations(    # Such as LLOD, meaningful max
      cc_se = calcurve_se, cal_0_se, quant_se, mat_id, c_concs
    )
    SumExp::row_df(calcurve_se) <- cbind(SumExp::row_df(calcurve_se), limit_df)
    SumExp::row_df(concn_se) <- cbind(SumExp::row_df(concn_se), limit_df)
    
    # Exclude the features by LLOQ and maximum concentration
    to_exclude <- SumExp::row_df(calcurve_se) |> 
      with(is.na(lloq) | is.na(llod) | (max_c_conc == -9) | is.na(max_c_conc))
    interm_data[["calcurve conc ranges"]] <- cbind(limit_df, to_exclude)
    calcurve_se <- calcurve_se[! to_exclude, ]
    concn_se <- concn_se[! to_exclude, ]
    
    # Replace the values outside the concentration range with NA
    lloq <- SumExp::row_df(calcurve_se)$lloq
    max_conc <- SumExp::row_df(calcurve_se)$max_c_conc
    calcurve_se[[mat_id]] <- proc$replace_outside_concentration_range_with_na(
      calcurve_se[[mat_id]], c_concs, min_conc = lloq, max_conc = max_conc
    )
    interm_data[["calcurve_se within range"]] <- calcurve_se
    
    # Fit the calibration curve
    rt_norm_mat <- calcurve_se[[mat_id]]
    calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
      proc$fit_and_test_calcurve_model(c_concs, rt_norm_mat[ii, ])
    })
    interm_data[["calcurve_models"]] <- calcurve_models
    
    # Compute the concentration of the samples using the calibration curve
    llodq <- SumExp::row_df(concn_se) |> 
      dplyr::select(lloq, llod)
    concn_se[["conc"]] <- proc$compute_concentration(
      concn_se, calcurve_se, calcurve_models, mat_id
    ) |> 
      labelled::set_label_attribute(
        paste0("Concentration [", user_inputs$concentration_unit, "]")
      ) |> 
      proc$replace_below_lloq_llod(llodq)
    labelled::label_attribute(concn_se) <- norm_lab
    # Maximum/minimum concentration after trimming out of each feature
    interm_data[["concn_se with conc"]] <- concn_se
    
    # Exclude the features with no quantification
    lloq <- SumExp::row_df(concn_se)$lloq
    non_qc_conc <- concn_se[, quote(! contr_cat %in% "QC")][["conc"]]
    any_above_lloq <- non_qc_conc > lloq
    concn_se <- concn_se[rowSums(any_above_lloq) > 0, ]
      
    list("conc" = concn_se, "interm_data" = interm_data)
  })
  # Store the intermediate data as a list
  proc$append_to_qc_steps(
    "calibration" = lapply(per_norm_lst, \(.x) .x$interm_data), 
    file = FILE$qc
  )
  
  # Save the processed data
  saveRDS(lapply(per_norm_lst, \(.x) .x$conc), file = FILE$proc)
  cat("The `SumExp` object after preprocessing saved to:", FILE$proc, "\n")
}
# Mark the preprocessing step as completed
proc$append_to_qc_steps("Preprocessing Completed" = TRUE, file = FILE$qc)
cat("The intermediate objects during preprocessing saved to:", FILE$qc, "\n")

