# ------------------------------------------------------------------------------------------- #
# Parse a MS-Dial file and save the data to an `rds` file
# ------------------------------------------------------------------------------------------- #

# Read `params.yml` to get input file
params <- yaml::read_yaml("params.yml")
stopifnot(!is.null(params$input_file))

# Load packages and project local libraries
options(box.path = "code/")           # Path to project local libraries
box::use(
  projlib/msdial,   # Handle MS-Dial files
  projlib/io,       # Handle snakemake parameters and input/output files
)
# Input/Output files
FILE <- list(i = params$input_file)
# Output file name, the base name of the input file with .rds extension
FILE$o <- msdial$get_sumexp_file_name(params)
# Check if input files and output directories exist
io$check_io_exist(FILE)

# Parse the Data into Elements of SumExp -------------------------------------------------

i_three_sections <- msdial$get_three_section_indices(FILE$i)
# Sample Info
sample_info <- msdial$fetch_sample_info(FILE$i, i_three_sections[[2]])

# Chemical Data
chems <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[[1]]) |>
  dplyr::select(        # SummarizedExperiment will `make.names` change anyhow
    alignment_id = "Alignment ID",
    chem_name = "Metabolite name",
    mz = "Average Mz",
    rt = "Average Rt(min)",
    sn_ratio = "S/N average",
    std_type = "Comment",            # Type of standard, e.g. "Quant", "IS", or NA 
  ) |> 
  # `chem_id` = Syntactically valid ID
  dplyr::mutate(chem_id = make.names(chem_name, unique = TRUE), .before = 1L) |> 
  dplyr::mutate(
    dplyr::across(c(mz, rt, sn_ratio), as.numeric),
    std_type = dplyr::case_when(
      std_type == "Quant" ~ "Quant",
      std_type == "IS"    ~ "IS",
      TRUE ~ ""              # Ignore other variants
    )
  )

# Measured values of the chemicals into a matrix
raw_df <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[[2]])
stopifnot(identical(colnames(raw_df), sample_info$given_sample_id))
colnames(raw_df) <- sample_info$sample_id     # Update with syntactically valid names
raw_mat <- lapply(raw_df, as.numeric) |> 
  as.data.frame(row.names = chems$chem_id) |> 
  as.matrix()

cat("From the given file:", FILE$i, "\n",
    "Number of samples:", nrow(raw_mat), "\n",
    "Number of chemicals:", nrow(raw_mat), "\n")

# Special control sample categories ------------------------------------------------------

sample_info <- sample_info |> 
  # Control sample groups
  dplyr::mutate(
    proc_cat = dplyr::case_when(
      sample_type == "Standard" & Class == "CalCurve" ~ "CalCurve",
      sample_type == "QC"    ~ "QC",
      sample_type == "Blank" ~ "Blank",
      TRUE ~ ""         # # NA does not behave predictably with `==`
    )
  )
stopifnot(
  "Calibration curve samples are required." = any(sample_info$proc_cat == "CalCurve"),
  "QC samples are required." = any(sample_info$proc_cat == "QC"),
  "Blank samples are required." = any(sample_info$proc_cat == "Blank")
)
# Function to find concentration values from the given IDs
find_concentration <- function(sid) {
  sid |> 
    stringr::str_extract("Cal_([[:digit:]-]+)", group = 1) |> 
    stringr::str_replace("-", ".") |>     # Replace "-" with "."
    as.numeric()
}
sample_info <- sample_info |> 
  dplyr::mutate(
    c_conc = ifelse(proc_cat == "CalCurve", find_concentration(given_sample_id), NA_real_),
    injection_order = as.integer(injection_order),
  )
calcurve_conc <- sample_info$c_conc[sample_info$proc_cat == "CalCurve"]
stopifnot(
  "Error in Calibration sample IDs" = all(!is.na(calcurve_conc)), 
  "`Cal_0` samples are required." = any(calcurve_conc == 0),
  "Multiple curve samples per concentration are required." = all(table(calcurve_conc) > 1)
)

# Into SummarizedExperiment
sumexp <- SummarizedExperiment::SummarizedExperiment(
  assays = list(raw = raw_mat),
  colData = sample_info,
  rowData = chems,
  metadata = rlang::list2(
    file_name = basename(FILE$i),
    file_md5 = digest::digest(FILE$i, algo = "md5", file = TRUE)
  )
)

# Save the SummarizedExperiment object
saveRDS(sumexp, file = FILE$o)
cat("SummarizedExperiment object saved to:", FILE$o, "\n")

