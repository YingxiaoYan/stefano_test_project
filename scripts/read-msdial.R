# ------------------------------------------------------------------------------------------- #
# Parse a MS-Dial file and save the data to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  projlib/msdial,   # Handle MS-Dial files
  io = projlib/check_io_exist,       # Check input/output files
)

# Read `params.yml` to get input file
params <- yaml::read_yaml("params.yml")
msdial$has_required_params(params, "input_file")
# Input/Output files
FILE <- list(i = params$input_file)
# Output file name, the base name of the input file with .rds extension
FILE$o <- msdial$get_sumexp_file_name(params, "")
# Check if input files and output directories exist
io$check_io_exist(FILE)

# Parse the Data into Elements of SumExp -------------------------------------------------

i_three_sections <- msdial$get_three_section_indices(FILE$i)
# Sample Info
sample_info <- msdial$fetch_sample_info(FILE$i, i_three_sections[[2]])

# Feature Data
features <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[[1]]) |>
  dplyr::select(          # new_id = "Given ID"
    alignment_id = "Alignment ID",
    feature_name = "Metabolite name",
    mz = "Average Mz",
    rt = "Average Rt(min)",
    sn_ratio = "S/N average",
    std_type = "Comment",            # Type of standard, e.g. "Quant", "IS", or NA 
  ) |> 
  as.data.frame()        # To have row names

# Syntactically valid ID
rownames(features) <- make.names(features$feature_name, unique = TRUE)
features <- features |> 
  dplyr::mutate(
    dplyr::across(c(mz, rt, sn_ratio), as.numeric),
    # Ignore other variants
    std_type = ifelse(std_type %in% c("Quant", "IS"), std_type, ""),
  )
# Apply labels
features <- features |> 
  labelled::set_variable_labels(
    alignment_id = "Alignment ID",
    feature_name = "Feature Name",
    mz = "Average M/Z",
    rt = "Average Retention Time (min)",
    sn_ratio = "Average S/N Ratio",
    std_type = "Standard Type"
  )

# Measured values of the features into a matrix
raw_df <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[[2]])
stopifnot(identical(colnames(raw_df), labelled::remove_labels(sample_info$sample_name)))
colnames(raw_df) <- rownames(sample_info)     # Update with syntactically valid names
raw_mat <- lapply(raw_df, as.numeric) |> 
  as.data.frame(row.names = rownames(features)) |> 
  as.matrix()
labelled::label_attribute(raw_mat) <- "Peak area"

cat("From the given file:", FILE$i, "\n",
    "Number of samples:", ncol(raw_mat), "\n",
    "Number of features:", nrow(raw_mat), "\n")

# Special control sample categories ------------------------------------------------------

sample_info <- sample_info |> 
  dplyr::mutate(
    # Control sample categories
    contr_cat = dplyr::case_when(
      sample_type == "Standard" & Class == "CalCurve" ~ "CalCurve",
      sample_type == "QC"    ~ "QC",
      sample_type == "Blank" ~ "Blank",
      TRUE ~ ""         # # NA does not behave predictably with `==`
    ) |> 
      labelled::set_label_attribute("Control Sample Category")
  )
for(cat in c("CalCurve", "QC", "Blank")) {
  if (!any(sample_info$contr_cat == cat)) stop("`", cat, "` samples are required.")
}
# Function to identify concentration values from the given IDs
catch_concentration <- function(sid) {
  sid |> 
    stringr::str_extract("Cal_([[:digit:]-]+)", group = 1) |> 
    stringr::str_replace("-", ".") |>     # Replace "-" with "."
    as.numeric()
}
sample_info <- sample_info |> 
  dplyr::mutate(
    c_conc = ifelse(contr_cat == "CalCurve", catch_concentration(sample_name), NA_real_) |> 
      labelled::set_label_attribute("Known Concentration"),
    injection_order = as.integer(injection_order) |> 
      labelled::copy_labels_from(injection_order),
  )
calcurve_conc <- sample_info$c_conc[sample_info$contr_cat == "CalCurve"]
stopifnot(
  "Error in Calibration sample IDs" = all(!is.na(calcurve_conc)), 
  "`Cal_0` samples are required." = any(calcurve_conc == 0),
  "Multiple curve samples per concentration are required." = all(table(calcurve_conc) > 1)
)

# Into "SumExp" class
sumexp <- SumExp::SumExp(
  raw = raw_mat,
  col_df = sample_info,
  row_df = features,
  metadata = rlang::list2(
    file_name = basename(FILE$i),
    file_md5 = digest::digest(FILE$i, algo = "md5", file = TRUE),
  )
)

# Save the "SumExp" object
saveRDS(sumexp, file = FILE$o)
cat("`SumExp` object saved to:", FILE$o, "\n")

