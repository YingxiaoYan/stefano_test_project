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
# Read `params.yml` to get various project parameters
params <- yaml::read_yaml("params.yml")

# Input/Output files
FILE <- list(
  o = rlang::list2(
    proc = msdial$get_sumexp_file_name(params, "proc"),
    qc = msdial$get_sumexp_file_name(params, "qc_steps"),   # Intermediate state of the data
  )
)
# Check if input files and output directories exist
box::use(io = projlib/check_io_exist)
io$check_io_exist(FILE)

# To store intermediate data during quality control steps. `append_to_qc_steps` will append
proc$initialize_qc_steps(FILE$o$qc)

# Read the parsed data by `read-msdial.R`. If not available, warn the user
overall_sumexp <- msdial$read_parsed_msdial_data(params)

# Conversion tables for presenting tables and figures
sample_id_name_tbl <- SumExp::col_df(overall_sumexp) |> 
  tibble::rownames_to_column("sample_id") |> 
  dplyr::select(sample_id, sample_name)
feature_id_name_tbl <- SumExp::row_df(overall_sumexp) |> 
  tibble::rownames_to_column("feature_id") |> 
  dplyr::select(feature_id, feature_name)
proc$append_to_qc_steps(
  "sample_id_name_tbl" = sample_id_name_tbl, 
  "feature_id_name_tbl" = feature_id_name_tbl,
  file = FILE$o$qc
)

# Normalization using internal standards -------------------------------------------------

# Extract internal standard features
internal_std_se <- local(
  envir = list(se = overall_sumexp),
  {
    se <- se[quote(std_type == "IS"), ]
    se[quote(order(rt)), ]    # Sort by average retention time
  }
)
# Store intermediate data during quality control steps
proc$append_to_qc_steps("internal std. raw" = internal_std_se, file = FILE$o$qc)

# Outlier removal     ---------------
# Number of outlying internal standard features per sample
n_outlier <- proc$count_outliers_per_sample(internal_std_se, mat_id = "raw", times = 3)
n_feature <- nrow(internal_std_se)
# Outlying samples
is_outlier <- n_outlier > (0.2 * n_feature)
proc$append_to_qc_steps(
  "internal std. number of outliers per sample" = n_outlier,
  "internal std. outlying samples" = is_outlier, 
  file = FILE$o$qc
)
if (any(is_outlier)) {
  # Exclude the outlying samples
  stopifnot(identical(names(is_outlier), colnames(internal_std_se)))
  internal_std_se <- internal_std_se[, !is_outlier]
  stopifnot(identical(names(is_outlier), colnames(overall_sumexp)))
  overall_sumexp <- overall_sumexp[, !is_outlier]
}

# Failed internal standards     ---------------
num_zeros <- proc$count_zeros_per_feature(internal_std_se[["raw"]])
is_failed <- num_zeros > 0
proc$append_to_qc_steps(
  "internal std. failed IS" = is_failed, 
  file = FILE$o$qc
)
# Remove failed internal standard features
internal_std_se <- internal_std_se[!is_failed, ]


# Normalize the data using closest internal standard features     ---------------
closest_istd <- proc$get_value_of_closest_istd(overall_sumexp, internal_std_se, "raw", "rt")
overall_sumexp[["closest_norm"]] <- (overall_sumexp[["raw"]] / closest_istd) |> 
  labelled::set_variable_labels("Closest RT Normalized")

# LOESS fit over RT normalization     ---------------
overall_rt_range <- range(SumExp::row_df(overall_sumexp)$rt)    # Fit for RT of all features
excl_cat <- c("Blank", "CalCurve")
loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1
)
# Normalize the data by LOESS fit along RT
rt <- SumExp::row_df(overall_sumexp)$rt
raw <- overall_sumexp[["raw"]]
overall_sumexp[["loess_norm"]] <- sapply(
  colnames(overall_sumexp), function(sample_id) {
    norm_factor <- predict(loess_fit[[sample_id]], newdata = rt)
    exp(log(raw[, sample_id]) - norm_factor)     # Normalize in log scale
  }
) |> 
  labelled::set_variable_labels("LOESS Normalized")
proc$append_to_qc_steps(
  "excluded categories in normalization" = excl_cat, 
  "LOESS fit" = loess_fit,
  "Normalized" = overall_sumexp, 
  file = FILE$o$qc
)

# Calibration using calcurve -------------------------------------------------------------

# Extract quantification standard data
quant_se <- overall_sumexp[quote(std_type == "Quant"), ]
# Before excluding out-of-range calibration concentrations
calcurve_se0 <- quant_se[, quote(contr_cat == "CalCurve")]
stopifnot("Calibration curve samples are required." = nrow(calcurve_se0) > 0)
# Calibration zero samples (Not modified during calibration)
cal_0_se <- calcurve_se0[, quote(c_conc == 0)] 
stopifnot("Multiple `Cal_0` samples are required." = ncol(cal_0_se) > 1)


# Per normalization method
concn_minus_blk_se_lst <- setNames(nm = c("loess_norm", "closest_norm")) |> 
  lapply(function(mat_id) {
    calcurve_se <- calcurve_se0[, quote(c_conc != 0)]          # Non-zero conc
    # Non-calcurve. `conc` will be added.
    concn_se <- quant_se[, quote(! contr_cat %in% "CalCurve")]
    SumExp::col_df(concn_se)$c_conc <- NULL     # Non-calcurve doesn't have known concentration
    stopifnot(identical(rownames(calcurve_se), rownames(concn_se)))  # Identical features
    
    # Store intermediate data during calibration. `append_to_qc_steps` takes too long to save
    interm_data <- list()
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
      dplyr::select(lloq, llod, llod_signal, lloq_signal)
    concn_se[["conc"]] <- proc$compute_concentration(
      concn_se, calcurve_se, calcurve_models, mat_id
    ) |> 
      proc$replace_below_lloq_llod(concn_se[[mat_id]], llodq)
    # Maximum/minimum concentration after trimming out of each feature
    interm_data[["concn_se with conc"]] <- concn_se
    
    # Blank subtraction
    concn_minus_blk_se <- proc$subtract_blank_sumexp(
      concn_se, contr_cat == "Blank", mat_id = "conc"
    )
    interm_data[["concn_minus_blk_se"]] <- concn_minus_blk_se
    labelled::label_attribute(interm_data) <- norm_lab
    # Wrap up the intermediate data as a list
    interm_data <- setNames(list(interm_data), mat_id)
    do.call(proc$append_to_qc_steps, c(interm_data, file = FILE$o$qc))
    
    concn_minus_blk_se |> 
      labelled::set_label_attribute(norm_lab)
  })

# Save the processed data
saveRDS(concn_minus_blk_se_lst, file = FILE$o$proc)
cat("The `SumExp` object after preprocessing saved to:", FILE$o$proc, "\n")
# Mark the preprocessing step as completed
proc$append_to_qc_steps("Preprocessing Completed" = TRUE, file = FILE$o$qc)
cat("The intermediate objects during preprocessing saved to:", FILE$o$qc, "\n")

