# ------------------------------------------------------------------------------------------- #
# Generate an artificial test dataset in TSV format
# ------------------------------------------------------------------------------------------- #

set.seed(2025)

# Sample information -----

N_SAMP <- rlang::list2(
  cc1 = 11 * 3,   # Calibration curve samples in batch 1
  qc1 = 9,    # QC samples in batch 1
  sm1 = 10,   # Study samples in batch 1
  blk1 = 3,   # Blank samples in batch 1
  cc2 = 8 * 2,    # Calibration curve samples in batch 2
  qc2 = 2,    # QC samples in batch 2
  sm2 = 5,    # Study samples in batch 2
  blk2 = 1    # Blank samples in batch 2 
)

sinfo <- tibble::tibble(
  `Class` = c(
    rep("CalCurve", N_SAMP$cc1),
    rep("QC_low", N_SAMP$qc1 / 3),
    rep("QC_mid", N_SAMP$qc1 / 3),
    rep("QC_high", N_SAMP$qc1 / 3),
    rep("Sample", N_SAMP$sm1),
    rep("Blank_1", N_SAMP$blk1),
    rep("calcurve", N_SAMP$cc2),
    rep("qc", N_SAMP$qc2),
    rep("Sample", N_SAMP$sm2),
    rep("Blank", N_SAMP$blk2)
  ),
  `File type` = c(
    rep("Standard", N_SAMP$cc1),
    rep("QC", N_SAMP$qc1),
    rep("Sample", N_SAMP$sm1),
    rep("Blank", N_SAMP$blk1),
    rep("Standard", N_SAMP$cc2),
    rep("QC", N_SAMP$qc2),
    rep("Sample", N_SAMP$sm2),
    rep("Blank", N_SAMP$blk2)
  ),
  `Injection order` = c(
    sample(sum(unlist(N_SAMP[1:4]))),  # Batch 1
    sample(sum(unlist(N_SAMP[5:8])))   # Batch 2
  ),
  `Batch ID` = c(
    rep(1, sum(unlist(N_SAMP[1:4]))),
    rep(2, sum(unlist(N_SAMP[5:8])))
  ),
  conc = c(
    rep(c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50), each = N_SAMP$cc1 / 11),
    rep(NA, N_SAMP$qc1 + N_SAMP$sm1 + N_SAMP$blk1),
    rep(c(0.02, 0.2, 0.5, 1, 2, 5, 10, 20), each = N_SAMP$cc2 / 8),
    rep(NA, N_SAMP$qc2 + N_SAMP$sm2 + N_SAMP$blk2)
  ),
  sample_id = dplyr::case_when(
    `File type` == "Standard" ~ stringr::str_replace_all(paste0("Cal", conc), "\\.", "-"),
    grepl("QC_", `Class`) ~ paste0("250910_", `Class`),
    `Class` == "Sample" ~ "Sample",
    grepl("Blank", `Class`) ~ paste0("Blank_", `Batch ID`),
    `Class` == "qc" ~ "qc",
  ),
)
sinfo <- sinfo |> 
  dplyr::mutate(
    sample_id = ifelse(
      `File type` == "Sample",
      paste0(sample_id, sprintf("_%02i", dplyr::row_number())),
      paste0(sample_id, "_", dplyr::row_number())
    ),
    .by = sample_id
  )

# Feature information -----
N_FEATURE <- 100
finfo <- tibble::tibble(
  `Alignment ID` = 1:N_FEATURE,
  `Average Rt(min)` = sort(rnorm(150, mean = 10, sd = 4))[26:125] |> 
    sample(N_FEATURE) |>
    round(3),
  `Average Mz` = 25 * `Average Rt(min)` + rnorm(N_FEATURE, mean = 70, sd = 100) |> round(5),
  `Metabolite name`	= "Feature_" |> paste0(1:N_FEATURE),
  `Comment` = sample(
    c("Quant", "IS", NA), 
    N_FEATURE, 
    replace = TRUE, 
    prob = c(0.55, 0.35, 0.1)
  )
)
finfo$`Comment`[which(finfo$`Comment` == "IS")[1L]] <- "vIS"

# Numeric data -----

# Variation parameters for each feature/sample
log_m <- abs(rnorm(N_FEATURE, mean = 8, sd = 1))    # Feature-specific mean on log10 scale
m_conc <- rnorm(N_FEATURE, mean = 0, sd = 1)
sample_mean <- rnorm(nrow(sinfo), mean = 0, sd = 0.2)  # Sample-specific variation

conc <- log10(sinfo$conc[!is.na(sinfo$conc)])

# Simulate a sigmoid relationship with noise for calibration curve samples
mat_calcurve <- sapply(1:N_FEATURE, function(ii) {
  c_y <- log_m[ii]    # Center of the sigmoid
  L <- c_y + rnorm(1, 2, 1)  # Maximum value (upper asymptote)
  x0 <- m_conc[ii]  # Midpoint (inflection point) in x-axis
  k <- runif(1, 0.3, 0.8)   # Steepness (slope)
  noise_sd <- 0.02  # Standard deviation of Gaussian noise

  y <- L / (1 + exp(-k * (conc - x0))) + rnorm(length(conc), 0, noise_sd)
  y + sample_mean[!is.na(sinfo$conc)]   # Add sample-specific variation
})

mat_qc <- sapply(log_m, function(m1) {
  is_qc <- sinfo$`File type` == "QC"
  y <- rnorm(sum(is_qc), mean = m1, sd = 0.02)
  y <- sample_mean[is_qc] + y   # Add sample-specific variation
  n_3 <- N_SAMP$qc1 / 3
  # Adjust to have three levels
  y[1:n_3] <- y[1:n_3] - 1  # QC_low
  y[(n_3 + 1):(2 * n_3)] <- y[(n_3 + 1):(2 * n_3)]      # QC_mid
  y[(2 * n_3 + 1):(3 * n_3)] <- y[(2 * n_3 + 1):(3 * n_3)] + 1  # QC_high
  y
})

mat_blank <- sapply(log_m, function(m1) {
  is_blank <- sinfo$`File type` == "Blank"
  rnorm(sum(is_blank), mean = m1 - 3, sd = 0.02) + sample_mean[is_blank]
})

mat_sample <- sapply(log_m, function(m1) {
  is_sample <- sinfo$`File type` == "Sample"
  rnorm(sum(is_sample), mean = m1, sd = 0.8) + sample_mean[is_sample]
})

# Combine numeric data of all samples -----
mat <- t(rbind(
  mat_calcurve[1:N_SAMP$cc1, ],
  mat_qc[1:N_SAMP$qc1, ],
  mat_sample[1:N_SAMP$sm1, ],
  mat_blank[1:N_SAMP$blk1, ],
  # Batch 2
  mat_calcurve[(N_SAMP$cc1 + 1):(N_SAMP$cc1 + N_SAMP$cc2), ],
  mat_qc[(N_SAMP$qc1 + 1):(N_SAMP$qc1 + N_SAMP$qc2), ],
  mat_sample[(N_SAMP$sm1 + 1):(N_SAMP$sm2 + N_SAMP$sm1), ],
  mat_blank[(N_SAMP$blk1 + 1):(N_SAMP$blk1 + N_SAMP$blk2), ]
))
# Adjust intensities of internal standards
is_int_std <- finfo$`Comment` %in% c("IS", "vIS")
v <- log_m[is_int_std] + rnorm(sum(is_int_std), 0, 0.3)
mat[is_int_std, ] <- v + matrix(
  rep(sample_mean + rnorm(ncol(mat), 0, 0.01), each = length(v)),
  nrow = length(v)
)
# Introduce a batch effect
mat[, sinfo$`Batch ID` == 2] <- mat[, sinfo$`Batch ID` == 2] + 0.1
mat <- 10^mat |> round(0)

# Combine all into a data frame -----

sinfo_1 <- sinfo |>
  dplyr::select(
    `Class`, 
    `File type`, 
    `Injection order`,
    `Batch ID`, 
  )
sinfo$sample_id <- stringr::str_replace_all(sinfo$sample_id, "Cal0-02", "Cal0")

finfo_txt <- sapply(finfo, as.character)

header <- cbind(
  finfo[NA, ][1:4, ],   # empty rows for header
  "colname" = colnames(sinfo_1),
  as.data.frame(t(sinfo_1))
) |>
  rbind(
    c(
      colnames(finfo),
      "sample_id",
      sinfo$sample_id
    )
  )
body <- cbind(
  finfo_txt,
  "colname" = NA,
  as.data.frame(mat)
)

rbind(header, body) |>
  readr::write_tsv("code/tests/testdata/testdata.tsv", na = "", col_names = FALSE)

