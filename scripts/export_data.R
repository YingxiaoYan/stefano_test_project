# ------------------------------------------------------------------------------------------- #
# Export 2 tables per normalization method:
# 1. Normalized data of overall features including non-targets
# 2. Blank subtracted normalized data of overall features
# 3. Concentration data of targeted features (blank subtracted)
# 
# Output file names are based on the input file name and normalization method.
# 1. (base name).(normalized method).norm.xlsx
# 2. (base name).(normalized method).norm_blk.xlsx
# 3. (base name).(normalized method).conc.xlsx
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  util = projlib/msdial_utils,        # Utility functions for MS-Dial data
  io = projlib/check_io_exist,
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir", "table_dir")

# Input files
FILE <- list(
  i = rlang::list2(
    raw = user_inputs$input_file,     # Copy unsaved feature info into output files
    # Intermediate status of the data
    to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
  )
)
io$chq_all_files_exist(FILE)
# Load the normalized data
to_report <- readRDS(FILE$i$to_rep)
stopifnot("Processing NOT completed." = to_report[["Completed"]])
mat_ids_to_export <- to_report[["matrix ids to blk subt"]]     # IDs of matrices to export
IS_TARGET_MODE <- !to_report[["is_non_target_mode"]]

if (IS_TARGET_MODE) {
  # Processed data
  FILE$i$proc <- msdial$get_raw_data_file_name(user_inputs, suffix = "proc")
  io$chq_all_files_exist(FILE$i$proc)
}

# Output files
FILE$o <- local({
  # Copy the basename of the input file to the output file names
  # "(base name).(normalized method).conc.xlsx", "().norm.xlsx" and "().norm_blk.xlsx"
  b <- basename(user_inputs$input_file) |> 
    tools::file_path_sans_ext()    # Without extension
  ns <- stringr::str_remove(mat_ids_to_export, "_norm")
  paste_file_name <- function(x, p) {
    file.path(user_inputs$table_dir, paste0(b, ".", x, ".", p, ".xlsx"))
  }
  out_data <- if (IS_TARGET_MODE) {
    c("norm", "norm_blk", "conc")
  } else {
    c("norm", "norm_blk")
  }
  lapply(setNames(nm = out_data), \(.x) {
    lapply(ns, paste_file_name, p = .x)
  })
})
io$mkdir_if_not_exist(dirname(unlist(FILE$o)))

# Export normalized data ----------
for (ii in seq_along(mat_ids_to_export)) {
  msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = to_report[["normalized"]], 
    mat_id = mat_ids_to_export[ii],
    in_file = FILE$i$raw,         # Copy feature information from the original MS-DIAL file
    out_file = FILE$o$norm[[ii]]
  )

  msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = to_report[["normalized - blank"]],
    mat_id = util$mat_id_of_blank_subtracted(mat_ids_to_export[ii]),
    in_file = FILE$i$raw,         # Copy feature information from the original MS-DIAL file
    out_file = FILE$o$norm_blk[[ii]]
  )
}

# Export concentration values ----------
if (IS_TARGET_MODE) {
  io$chq_all_files_exist(FILE$i$proc)
  # Load the concentration data
  concn_lst <- readRDS(FILE$i$proc)

  for (ii in seq_along(mat_ids_to_export)) {
    mat_id <- mat_ids_to_export[ii]
    # The ID of the matrix to use for calibration
    mat_id_for_calib <- mat_id |>
      util$mat_id_of_blank_subtracted() |>
      util$mat_id_for_calibration()
    # Load the processed data using the specified normalization method
    lst_proc <- lapply(concn_lst, \(.x) .x[[mat_id_for_calib]])    # per batch
    lst_proc <- lapply(lst_proc, \(ea) ea[["concn"]])   # Extract concentration data
    msdial$export_concentration_xlsx(
      # Concentration has been computed on the blank subtracted data
      sumexp_lst = lst_proc,
      file = FILE$o$conc[[ii]],
      is_closest_norm = mat_id == "closest_norm"
    )
  }
}

cat("Tables have been saved to:\n")      # Avoid a leading space
cat(paste(unlist(FILE$o), collapse = "\n"), "\n")
