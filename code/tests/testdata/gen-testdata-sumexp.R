# ------------------------------------------------------------------------------------------- #
# Generate an artificial test dataset
# ------------------------------------------------------------------------------------------- #

# "params.yml": The parameter file in YAML format that stores user's MS-DIAL info
.yml_file <- "params.yml"

old_user_params <- yaml::read_yaml(.yml_file)

# User project parameters
user_params <- rlang::list2(
  "project_title" = "Test data",
  "input_file" = "code/tests/testdata/testdata.tsv",
  "intermediate_dir" = "code/tests/testdata",
  "table_dir" = "data/processed",
  "report_dir" = "reports",
  "concentration_unit" = "ng/ml",
  "user" = "",
  "university" = "",
  "free_text" = "",
)

yaml::write_yaml(user_params, file = .yml_file)

source("code/scripts/read-msdial.R")   # Read MS-Dial output file

# Run processing using Rscript
system(paste(
  "Rscript code/scripts/process.R",
  "--rm_outlier TRUE", 
  "--log_calibration TRUE", 
  "--weight 1", 
  "--llox_method pt_signal_mean_plus_sd"
))

yaml::write_yaml(old_user_params, file = .yml_file)   # Restore old params
