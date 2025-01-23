# ------------------------------------------------------------------------------------------- #
# Export 2 tables per normalization method:
# 1. Normalized data of overall features including non-targets
# 2. Blank subtracted normalized data of overall features
# 3. Concentration data of targeted features (blank subtracted)
# 
# Output file names are based on the input file name and normalization method.
# 1. (base name).(normalized method).norm.tsv
# 2. (base name).(normalized method).norm_blk.tsv
# 3. (base name).(normalized method).conc.tsv
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  io = projlib/check_io_exist,
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir", "table_dir")

# Input files
FILE <- list(
  i = rlang::list2(
    raw = user_inputs$input_file,     # Copy unsaved feature info into output files
    # Processed data
    proc = msdial$get_raw_data_file_name(user_inputs, suffix = "proc"),
    # Intermediate status of the data
    qc = msdial$get_raw_data_file_name(user_inputs, suffix = "qc_steps"),
  )
)
io$chq_all_files_exist(FILE$i[c("raw", "qc")])
# Load the normalized data
qc_steps <- readRDS(FILE$i$qc)
stopifnot("Preprocessing NOT completed." = qc_steps[["Preprocessing Completed"]])
norm_mat_ids <- qc_steps[["normalized matrix ids"]]
norm_blk_mat_ids <- qc_steps[["normalized blank subtracted matrix ids"]]
raw_se <- msdial$read_parsed_msdial_data(user_inputs)
is_non_target_mode <- SumExp::metadata(raw_se)$is_non_target_mode
# Output files
FILE$o <- local({
  # Copy the basename of the input file to the output file names
  # "(base name).(normalized method).conc.tsv", "().norm.tsv" and "().norm_blk.tsv"
  b <- basename(user_inputs$input_file) |> 
    tools::file_path_sans_ext()    # Without extension
  ns <- stringr::str_remove(norm_mat_ids, "_norm")
  paste_file_name <- function(x, p) {
    file.path(user_inputs$table_dir, paste0(b, ".", x, ".", p, ".tsv"))
  }
  out_data <- if (is_non_target_mode) {
    c("norm", "norm_blk")
  } else {
    c("norm", "norm_blk", "conc")
  }
  lapply(setNames(nm = out_data), \(.x) {
    lapply(ns, paste_file_name, p = .x)
  })
})
io$mkdir_if_not_exist(dirname(unlist(FILE$o)))

for(ii in seq(norm_mat_ids)) {
  msdial$export_data_with_feature_table_tsv(
    sumexp = qc_steps[["normalized"]], 
    mat_id = norm_mat_ids[ii],
    in_file = FILE$i$raw,         # Copy feature information from the original MS-DIAL file
    out_file = FILE$o$norm[[ii]]
  )

  msdial$export_data_with_feature_table_tsv(
    sumexp = qc_steps[["normalized - blank"]],
    mat_id = norm_blk_mat_ids[ii],
    in_file = FILE$i$raw,         # Copy feature information from the original MS-DIAL file
    out_file = FILE$o$norm_blk[[ii]]
  )
}

if (!is_non_target_mode) {
  io$chq_all_files_exist(FILE$i$proc)
  # Load the concentration data
  concn_lst <- readRDS(FILE$i$proc)

  for(ii in seq(norm_mat_ids)) {
    msdial$export_concentration_tsv(
      sumexp = concn_lst[[ norm_blk_mat_ids[ii] ]],
      file = FILE$o$conc[[ii]]
    )
  }
}

cat("Tables have been saved to:\n")      # Avoid a leading space
cat(paste(unlist(FILE$o), collapse = "\n"), "\n")
