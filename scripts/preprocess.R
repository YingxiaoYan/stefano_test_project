# ------------------------------------------------------------------------------------------- #
# Parse a MS-Dial file, preprocess, and save the data to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  projlib/proc,       # Preprocessing functions
  projlib/io,       # Check input/output files
)
# Read `params.yml` to get input file
params <- yaml::read_yaml("params.yml")
msdial$has_required_params(params, "input_file", "intermediate_dir")  # read_parsed_msdial_data

# Input/Output files
FILE <- list(
  o = rlang::list2(
    proc = msdial$get_sumexp_file_name(params, "proc"),
    qc = msdial$get_sumexp_file_name(params, "qc_steps"),   # Intermediate state of the data
  )
)
# Check if input files and output directories exist
io$check_io_exist(FILE)
proc$initialize_qc_steps(FILE$o$qc)     # To store intermediate data during quality control steps

# Read the parsed data by `read-msdial.R`
overall_sumexp <- msdial$read_parsed_msdial_data(params)

# Extract internal standard chemicals
internal_std_se <- local({
  se <- se[quote(std_type == "IS"), ]
  se[quote(order(rt)), ]    # Sort by average retention time
}, list(se = overall_sumexp))
# Store intermediate data during quality control steps
proc$append_to_qc_steps("internal std. raw" = internal_std_se, file = FILE$o$qc)

# Number of outlying internal standard chemicals per sample      ---------------
n_out_istd <- proc$count_outliers_per_sample(internal_std_se, mat_id = "raw", times = 3)
n_chem <- nrow(internal_std_se)
is_outlier <- n_out_istd > (0.2 * n_chem)
proc$append_to_qc_steps(
  "internal std. number of outliers" = n_out_istd,
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
num_zeros <- proc$count_zeros_per_chemical(internal_std_se[["raw"]])
stopifnot(identical(names(num_zeros), rownames(internal_std_se)))
is_failed <- num_zeros > 0
proc$append_to_qc_steps("internal std. failed IS" = is_failed, file = FILE$o$qc)
failed_istd <- internal_std_se[is_failed, ]
# Remove failed internal standard chemicals
internal_std_se <- internal_std_se[!is_failed, ]


# Normalize the data using closest internal standard chemicals     ---------------
closest_istd <- proc$get_value_of_closest_istd(overall_sumexp, internal_std_se, "raw", "rt")
overall_sumexp[["closest_norm"]] <- (overall_sumexp[["raw"]] / closest_istd) |> 
  labelled::set_variable_labels("Closest RT Normalized")

# Find the LOESS fit for normalization     ---------------
overall_rt_range <- range(SumExp::row_df(overall_sumexp)$rt)    # Fit for RT of all chemicals
excl_cat <- c("Blank", "CalCurve")
loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1
)
proc$append_to_qc_steps(
  "excluded categories in normalization" = excl_cat, 
  "LOESS fit" = loess_fit,
  file = FILE$o$qc
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
proc$append_to_qc_steps("Normalized" = overall_sumexp, file = FILE$o$qc)

# Calibration     ---------------

# Extract quantification standard data
quant_se <- overall_sumexp[quote(std_type == "Quant"), ]

# Calibration curve samples only
calcurve_se0 <- quant_se[, quote(proc_cat == "CalCurve")]
stopifnot("Calibration curve samples are required." = nrow(calcurve_se0) > 0)
# Calibration zero samples
cal_0_se <- calcurve_se0[, quote(c_conc == 0)] 
stopifnot("Multiple `Cal_0` samples are required." = ncol(cal_0_se) > 1)

# Per normalization method     ---------------

concn_blank_substracted_lst <- 
  lapply(setNames(nm = c("loess_norm", "closest_norm")), function(mat_id) {
    calcurve_se <- calcurve_se0[, quote(c_conc != 0)]          # Non-zero conc
    concn_se <- quant_se[, quote(! proc_cat %in% "CalCurve")]   # Non-calcurve
    SumExp::col_df(concn_se)$c_conc <- NULL
    stopifnot(identical(rownames(calcurve_se), rownames(concn_se)))
    
    # Store intermediate data during calibration 
    interm_data <- list()
    # Label for the normalized data. To label the output of this function
    norm_lab <- labelled::get_label_attribute(quant_se[[mat_id]]) |> 
      stringr::str_replace("Normalized", "normalization")
    
    concs <- SumExp::col_df(calcurve_se)$c_conc   # Available concentrations
    limit_df <- proc$identify_limts_in_calibrations(
      cc_se = calcurve_se, cal_0_se, quant_se, mat_id, concs
    )
    SumExp::row_df(calcurve_se) <- cbind(SumExp::row_df(calcurve_se), limit_df)
    SumExp::row_df(concn_se) <- cbind(SumExp::row_df(concn_se), limit_df)
    
    # Exclude the chemicals by LLOQ and maximum concentration
    to_exclude <- SumExp::row_df(calcurve_se) |> 
      with(is.na(lloq) | is.na(llod) | (max_c_conc == -9) | is.na(max_c_conc))
    interm_data[["limits"]] <- cbind(limit_df, to_exclude)
    
    calcurve_se <- calcurve_se[! to_exclude, ]
    concn_se <- concn_se[! to_exclude, ]
    # Exclude the values outside the concentration range
    lloq <- SumExp::row_df(calcurve_se)$lloq
    max_conc <- SumExp::row_df(calcurve_se)$max_c_conc
    calcurve_se[[mat_id]] <- proc$replace_outside_concentration_range_with_na(
      calcurve_se[[mat_id]], concs, lloq, max_conc
    )
    interm_data[["calcurve_se within range"]] <- calcurve_se
    
    # Fit the calibration curve
    rt_norm_mat <- calcurve_se[[mat_id]]
    calcurve_models <- lapply(setNames(nm = rownames(calcurve_se)), function(ii) {
      proc$fit_and_test_calcurve_model(concs, rt_norm_mat[ii, ])
    })
    interm_data[["calcurve_models"]] <- calcurve_models
    
    # Compute the concentration of the samples using the calibration curve
    limit_df <- SumExp::row_df(concn_se) |> 
      dplyr::select(lloq, llod, llod_signal, lloq_signal)
    concn_se[["conc"]] <- proc$compute_concentration(
      concn_se, calcurve_se, calcurve_models, mat_id
    ) |> 
      proc$replace_below_lloq_llod(concn_se[[mat_id]], limit_df)
    # Maximum/minimum concentration after trimming out of each chemical
    interm_data[["concn_se within range"]] <- concn_se
    
    # Blank subtraction
    concn_blank_substracted <- proc$subtract_blank_sumexp(
      concn_se, proc_cat == "Blank", mat_id = "conc"
    )
    interm_data[["concn_blank_substracted"]] <- concn_blank_substracted
    labelled::label_attribute(interm_data) <- norm_lab
    do.call(proc$append_to_qc_steps, c(setNames(list(interm_data), mat_id), file = FILE$o$qc))
    
    concn_blank_substracted |> 
      labelled::set_label_attribute(norm_lab)
  })

# Save the processed data
saveRDS(concn_blank_substracted_lst, file = FILE$o$proc)
cat("The `SumExp` object after preprocessing saved to:", FILE$o$proc, "\n")
# Mark the preprocessing step as completed
proc$append_to_qc_steps("Preprocessed" = TRUE, file = FILE$o$qc)
cat("The intermediate objects during preprocessing saved to:", FILE$o$qc, "\n")

