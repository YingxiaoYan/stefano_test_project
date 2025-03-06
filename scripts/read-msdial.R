# ------------------------------------------------------------------------------------------- #
# Parse a MS-Dial file and save the data to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  projlib/msdial,   # Handle MS-Dial files
  io = projlib/check_io_exist,       # Check input/output files
)

# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir")
# Input/Output files
FILE <- list(i = user_inputs$input_file)
# Output file name, the base name of the input file with .rds extension
FILE$o <- msdial$get_raw_data_file_name(user_inputs)
# Check if input files and output directories exist
io$check_io_exist(FILE)
# When there is missing information for calibration of targeted chemicals
is_non_target_mode <- FALSE

# Parse the Data into Elements of SumExp -------------------------------------------------

i_three_sections <- msdial$get_three_section_indices(FILE$i)
# Sample Info
sample_info <- msdial$fetch_sample_info(FILE$i, i_three_sections[["2nd"]])

# Feature Data
features <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[["1st"]]) |>
  dplyr::select(   # Syntactically valid column names in R,  new_id = "Given ID"
    alignment_id = "Alignment ID",
    feature_name = "Metabolite name",
    mz = "Average Mz",
    rt = "Average Rt(min)",
    sn_ratio = "S/N average",
    std_type = "Comment",            # Type of standard, e.g. "Quant", "IS", "vIS", or NA 
  ) |> 
  as.data.frame()        # To have row names

# Syntactically valid ID
rownames(features) <- make.names(features$feature_name, unique = TRUE)
features <- features |> 
  dplyr::mutate(
    dplyr::across(c(mz, rt, sn_ratio), as.numeric),
    # Ignore other variants
    std_type = ifelse(std_type %in% c("Quant", "IS", "vIS"), std_type, ""),
  )
# Apply labels for plots and tables
features <- features |> 
  labelled::set_variable_labels(
    alignment_id = "Alignment ID",
    feature_name = "Feature Name",
    mz = "Average M/Z",
    rt = "Average Retention Time (min)",
    sn_ratio = "Average S/N Ratio",
    std_type = "Standard Type"
  )
stopifnot("`IS` features are required." = any(features$std_type == "IS"))
if (!any(features$std_type == "Quant")) {
  warning("`Quant` features are missing.")
  is_non_target_mode <- TRUE
}

# Measured values of the features into a matrix
raw_df <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[["2nd"]])
stopifnot(identical(colnames(raw_df), labelled::remove_labels(sample_info$sample_name)))
colnames(raw_df) <- rownames(sample_info)     # Update with syntactically valid names
raw_mat <- lapply(raw_df, as.numeric) |> 
  as.data.frame(row.names = rownames(features)) |> 
  as.matrix()
labelled::label_attribute(raw_mat) <- "Raw"

cat("From the given file:", FILE$i, "\n",
    "Number of samples:", ncol(raw_mat), "\n",
    "Number of features:", nrow(raw_mat), "\n")

# Special control sample categories ------------------------------------------------------

sample_info <- sample_info |> 
  dplyr::mutate(
    # Control sample categories
    contr_cat = dplyr::case_when(
      sample_type == "Standard" & 
        stringr::str_detect(sample_name, "Cal_[[:digit:]-]") ~ "CalCurve",
      sample_type == "QC"    ~ "QC",
      sample_type == "Blank" ~ "Blank",
      TRUE ~ ""         # # NA does not behave predictably with `==`
    ) |> 
      labelled::set_label_attribute("Control Sample Category")
  )
stopifnot("`Blank` samples are required." = any(sample_info$contr_cat == "Blank"))
for(cat in c("CalCurve", "QC")) {
  if (!any(sample_info$contr_cat == cat)) {
    warning("`", cat, "` samples are missing.")
    is_non_target_mode <- TRUE
  }
}
if (is_non_target_mode) {
  warning("Hence, the processing of the data will be stopped before calibration!")
} else {
  # Function to identify concentration values from the given IDs
  catch_concentration <- function(sid) {
    sid |> 
      stringr::str_extract("Cal_([[:digit:]-]+)", group = 1) |> 
      stringr::str_replace("-", ".") |>     # Replace "-" with "."
      as.numeric()
  }
  # Known concentration of calibration curves, which have been saved into the `sample_info`
  nm <- msdial$CALCURVE_CONCENTRATION_COLNAME
  sample_info[[nm]] <- ifelse(sample_info$contr_cat == "CalCurve", 
                              catch_concentration(sample_info$sample_name), 
                              NA_real_) |> 
    labelled::set_label_attribute("Known Concentration"),
  cc_conc <- sample_info[[nm]][sample_info$contr_cat == "CalCurve"]
  stopifnot(
    "Error in Calibration sample IDs" = all(!is.na(cc_conc)), 
    "Multiple curve samples per concentration are required." = all(table(cc_conc) > 1)
  )
}

sample_info <- sample_info |> 
  dplyr::mutate(
    injection_order = as.integer(injection_order) |> 
      labelled::copy_labels_from(injection_order),
  )

# Into "SumExp" class
sumexp <- SumExp::SumExp(
  raw = raw_mat,
  col_df = sample_info,
  row_df = features,
  metadata = rlang::list2(
    file_name = basename(FILE$i),
    file_md5 = digest::digest(FILE$i, algo = "md5", file = TRUE),
    is_non_target_mode = is_non_target_mode,
  )
)

# Save the "SumExp" object
saveRDS(sumexp, file = FILE$o)
cat("`SumExp` object saved to:", FILE$o, "\n")

