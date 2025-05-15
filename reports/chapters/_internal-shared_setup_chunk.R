# ------------------------------------------------------------------------------------------- #
# Shared setup chunk used across multiple Quarto markdown files for `report-ineternal.qmd`
# - Requires: 
#   - `params$norm_method`
# ------------------------------------------------------------------------------------------- #
DATA_ID <- paste0(params$norm_method, "_blk")

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,             # Light SummarizedExperiment, to use `[`
  util = projlib/msdial_utils,        # Utility functions for MS-Dial data
  projlib/msdial,     # Handle MS-Dial files
  projlib/show,       # Functions to present data
  ggplot2[...],        # Too long to write and usually specific enough
  io = projlib/check_io_exist,       # Check input/output files
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input()
# To keep constants consistent across reports
constants_yml <- yaml::read_yaml("code/constants.yml")
COLORS <- lapply(constants_yml$COLORS, unlist)    # Lists to named vectors
raw_se <- msdial$read_parsed_msdial_data(user_inputs)
# For consistency throughout the report
COLORS_OF_CLASSES <- show$get_colors_of_classes(raw_se, COLORS$.ctrl_cat)

# Input/Output files
FILE <- list(
  i = rlang::list2(
    # Processed data
    proc = msdial$get_raw_data_file_name(user_inputs, suffix = "proc"),
    # Intermediate status of the data
    qc = msdial$get_raw_data_file_name(user_inputs, suffix = "qc_steps"),
  )
)
# Check if input files and output directories exist
io$check_io_exist(FILE)

# Load the intermediate data during QC
qc_steps <- readRDS(FILE$i$qc)
stopifnot("Processing NOT completed." = qc_steps[["Completed"]])
calib_interm_data <- qc_steps[[paste0("calibration/", DATA_ID)]]

# Conversion tables between IDs (syntactically acceptable) and names (human-readable)
# These are used for presenting tables and figures in reports
sample_id_name_tbl <- SumExp::col_df(raw_se) |> 
  tibble::rownames_to_column("sample_id") |> 
  dplyr::select(sample_id, sample_name)
feature_id_name_tbl <- SumExp::row_df(raw_se) |> 
  tibble::rownames_to_column("feature_id") |> 
  dplyr::select(feature_id, feature_name)
