# ------------------------------------------------------------------------------------------- #
# Export 2 tables per normalization method:
# 1. Normalized data of overall features including non-targets
# 2. Concentration data of targeted features
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  projlib/proc[subtract_blank_sumexp], 
)
# Read `params.yml` to get input file
params <- yaml::read_yaml("params.yml")
msdial$has_required_params(params, "input_file", "intermediate_dir", "table_dir")

NORMALIZE_METHODS <- c("loess_norm", "closest_norm")

# Input/Output files
FILE <- list(
  i = rlang::list2(
    raw = params$input_file,     # Copy unsaved feature info into output files
    proc = msdial$get_sumexp_file_name(params, "proc"),
    qc = msdial$get_sumexp_file_name(params, "qc_steps"),   # Intermediate state of the data
  ),
  o = local({
    b <- basename(params$input_file) |> 
      tools::file_path_sans_ext()    # Without extension
    ns <- NORMALIZE_METHODS
    ns <- setNames(stringr::str_remove(ns, "_norm"), ns)
    rlang::list2(
      conc = lapply(ns, \(.x) file.path(params$table_dir, paste0(b, ".", .x, ".conc.tsv"))),
      norm = lapply(ns, \(.x) file.path(params$table_dir, paste0(b, ".", .x, ".norm.tsv"))),
    )
  })
)
# Check if input files and output directories exist
box::use(io = projlib/check_io_exist)
io$check_io_exist(FILE)

# Load the normalized data
qc_steps <- readRDS(FILE$i$qc)
stopifnot("Preprocessing was not completed." = qc_steps[["Preprocessing Completed"]])
normalized_se <- qc_steps[["Normalized"]]
# Load the concentration data
measurement_blank_substracted_lst <- readRDS(FILE$i$proc)

for(mat_id in NORMALIZE_METHODS) {
  subtract_blank_sumexp(normalized_se, contr_cat == "Blank", mat_id = mat_id) |> 
    msdial$export_data_with_feature_table_tsv(
      sumexp = _, 
      mat_id = mat_id,
      in_file = FILE$i$raw,         # Copy feature information from the original MS-DIAL file
      out_file = FILE$o$norm[[mat_id]]
    )
  msdial$export_concentration_tsv(
    sumexp = measurement_blank_substracted_lst[[mat_id]],
    file = FILE$o$conc[[mat_id]]
  )
}

cat("Tables have been saved to:\n")      # Avoid a leading space
cat(paste(unlist(FILE$o), collapse = "\n"), "\n")
