# ------------------------------------------------------------------------------------------- #
# Shared setup chunk used across multiple Quarto markdown files for `report-ineternal.qmd`
# - Requires:
#   - `params$norm_method`
# ------------------------------------------------------------------------------------------- #
setwd("..")
# Load packages and project local libraries
options(box.path = "code/") # Path to project local libraries
box::use(
  SumExp, # Light SummarizedExperiment, to use `[`
  util = projlib / msdial_utils, # Utility functions for MS-Dial data
  projlib / msdial, # Handle MS-Dial files
  projlib / show, # Functions to present data
  ggplot2[...], # Too long to write and usually specific enough
  io = projlib / check_io_exist, # Check input/output files
)







params_yml <- yaml::read_yaml("params.yml")

# Get the input file name provided by the user
##################################### here is the issue that it is using msdial output to generate raw se
user_inputs <- msdial$get_user_input()
# To keep constants consistent across reports

#print(params_yml$input_file)
#print(user_inputs)
#user_inputs<-params_yml

#user_inputs$input_file<-params_yml$input_file

constants_yml <- yaml::read_yaml("code/constants.yml")
COLORS <- lapply(constants_yml$COLORS, unlist) # Lists to named vectors
raw_se <- msdial$read_parsed_msdial_data(user_inputs)

print(c(dim(raw_se)[1],"afsfassffasfasfsofjsoas"))

IS_TARGET_MODE <- !SumExp::metadata(raw_se)$is_non_target_mode
# For consistency throughout the report
COLORS_OF_CLASSES <- show$get_colors_of_classes(raw_se, COLORS$.ctrl_cat)



# Conversion tables between IDs (syntactically acceptable) and names (human-readable)
# These are used for presenting tables and figures in reports
sample_id_name_tbl <- SumExp::col_df(raw_se) |>
  tibble::rownames_to_column("sample_id") |>
  dplyr::select(sample_id, sample_name)
feature_id_name_tbl <- SumExp::row_df(raw_se) |>
  tibble::rownames_to_column("feature_id") |>
  dplyr::select(feature_id, feature_name)


#########################################################################################
########################################################################################
## This is the data for section 4
params_yml$norm_method<-"loess_norm"
# The ID of the matrix to use in calibration
MAT_ID_BLANK_SUBT <- params_yml$norm_method |>
  util$mat_id_of_blank_subtracted()
MAT_ID_IN_CALIB <- util$mat_id_in_calibration(MAT_ID_BLANK_SUBT)


# Input/Output files
FILE <- list(
  i = rlang::list2(
    # Intermediate status of the data
    to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
    #to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
  )
)



# Load the intermediate data during QC
to_report <- readRDS(FILE$i$to_rep)
stopifnot("Processing NOT completed." = to_report[["Completed"]])
pre_norm_se <- to_report[["before normalization"]]

if (IS_TARGET_MODE) {
  # Processed data
  FILE$i$proc <- msdial$get_raw_data_file_name(user_inputs, suffix = "proc")
  io$check_io_exist(FILE)
  
  print(c(FILE$i$proc,"IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII"))
  
  # Load the processed data using the specified normalization method
  lst_proc <- readRDS(FILE$i$proc)[[MAT_ID_BLANK_SUBT]]
} else {
  io$check_io_exist(FILE)
}

setwd("code/")