box::use(./msdial)             # extract_qc_samples_to_list

#' Randomly select `n` rows from `sumexp` with optional `subset`
#'
#' @param sumexp a SummarizedExperiment object
#' @param n the number of rows to select
#' @param subset an optional subset expression to apply to the `rowData` of `sumexp`
#' @param keep_the_order if `TRUE`, the selected rows are sorted in the previous order
#'
#' @return a SummarizedExperiment object with `n` rows randomly selected from `sumexp`
#' @export
select_rows_randomly <- function(sumexp, n, subset, keep_the_order = FALSE) {
  ii <- c(1:nrow(sumexp))
  if (!missing(subset)) {      # If given, apply it to the `rowData` of `sumexp`
    ii <- ii[eval(substitute(subset), as.data.frame(SummarizedExperiment::rowData(sumexp)))]
  }
  ii <- sample(ii, n)
  if (keep_the_order) ii <- sort(ii)
  sumexp[ii, ]
}


#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param digits An integer for rounding
#' @return A numeric value of RSD%
#' @export
rsd_perc <- function(x, digits = 2) {
  round(100 * stats::sd(x) / mean(x), digits)
}

#' Number of zeros
#'
#' @param se A SummarizedExperiment object 
#' @param assay The name of an assay 
#' @return A SummarizedExperiment object with a new column `num_zeros` in the `rowData`
#' @export
add_num_zeros <- function(se, assay = "raw") {
  n0 <- rowSums(SummarizedExperiment::assay(se, assay) == 0)
  SummarizedExperiment::rowData(se)$num_zeros <- n0
  return(se)
}

#' RSD% across all samples
#'
#' @param se A SummarizedExperiment object 
#' @param assay The name of an assay
#' @return A SummarizedExperiment object with a new column `rsd` in the `rowData`
#' @export
add_rsd <- function(se, assay = "raw") {
  rsd <- apply(SummarizedExperiment::assay(se, assay), 1, rsd_perc)
  SummarizedExperiment::rowData(se)$rsd <- rsd
  return(se)
}


#' Identify outliers
#'
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @param x A numeric vector
#'
#' @return A logical vector indicating whether each element is an outlier
#' @export
identify_outliers <- function(x, times = 3) {
  m <- mean(x)
  sd <- stats::sd(x)
  return((x > m + times * sd) | (x < m - times * sd))
}



#' Count outlying internal standard chemicals per sample 
#'
#' @param se A SummarizedExperiment object
#' @param assay The name of an assay
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @return A numeric vector of the number of outlying internal standard chemicals per sample
#' @export
count_outliers_per_sample <- function(se, assay = "raw", times = 3) {
  x <- SummarizedExperiment::assay(se, assay)
  x <- log1p(x)              # Log-transform the data
  outlying <- apply(x, 1, identify_outliers, times = times)
  return(rowSums(outlying))     # Transposed by `apply` above
}



#' Get the values of the closest internal standard chemicals
#'
#' @param se A SummarizedExperiment object
#' @param istd_se A SummarizedExperiment object of internal standard chemicals
#' @param assay The name of an assay 
#' @param rt The name of the retention time column 
#' @return A numeric matrix of the values of the closest internal standard chemicals
#' @export
get_raw_of_closest_istd <- function(se, istd_se, assay = "raw", rt = "rt") {
  rt_x <- SummarizedExperiment::rowData(se)[[rt]]
  rt_istd <- SummarizedExperiment::rowData(istd_se)[[rt]]
  # Find the closest internal standard chemical
  i_closest <- sapply(rt_x, \(.x) which.min(abs(rt_istd - .x)))
  out <- SummarizedExperiment::assay(istd_se, assay)[i_closest, ]
  rownames(out) <- rownames(se)
  return(out)
}

#' Get the LOESS fit model
#'
#' @param istd_se A SummarizedExperiment object of internal standard chemicals
#' @param excl_cat A character vector of the categories to exclude
#' @param overall_rt_range A numeric vector of the overall retention time range, to which the model is expanded
#' @param span A numeric value for the span of the LOESS fit
#' @param assay The name of an assay
#' @param rt The name of the retention time column
#'
#' @return A list of the LOESS fit models
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, assay = "raw", rt = "rt") {
  # Log-transform the data
  istd_log <- log(SummarizedExperiment::assay(istd_se, assay))
  rt_istd <- SummarizedExperiment::rowData(istd_se)[[rt]]
  
  # Normalize the data using the internal standards
  # Mean of each internal standard chemical for overall measurement samples
  # , excluding the calibration curve and blank samples
  to_excl <- istd_se$proc_cat %in% excl_cat
  # The data of the samples with measurements
  mat <- istd_log[, !to_excl, drop = FALSE]
  stopifnot("Failed interncal control(s)! value = 0" = all(is.finite(mat)))
  istd_m <- rowMeans(mat)
  # Subtract the mean from the data
  fr_m_log <- istd_log - istd_m
  
  # Expand to smallest RT 
  istd_smallest_rt <- fr_m_log[which.min(rt_istd), ]
  # Expand to largest RT 
  istd_largest_rt <- fr_m_log[which.max(rt_istd), ]
  # Add the expanded range
  fr_m_log <- rbind(
    istd_smallest_rt, 
    istd_largest_rt,
    fr_m_log
  )
  
  # LOESS fit of the internal standard chemicals along RT
  loess_fit <- purrr::map(1:ncol(fr_m_log), function(ii) {
    # Fit a loess curve
    y <- fr_m_log[, ii]
    y[is.finite(y) == FALSE] <- NA
    stopifnot(identical(
      names(y), c("istd_smallest_rt", "istd_largest_rt", rownames(istd_se))
    ))
    suppressWarnings(stats::loess(y ~ c(overall_rt_range, rt_istd), span = span))
  })
  names(loess_fit) <- colnames(fr_m_log)
  return(loess_fit)
}


#' Extract quantitative standards in QC samples
#'
#' @param sumexp A SummarizedExperiment object
#' @return A list of SummarizedExperiment objects with quantitative standards in QC samples
#' @export
extract_quant_qc <- function(sumexp) {
  se_lst <- msdial$extract_qc_samples_to_list(sumexp)
  lapply(se_lst, \(se) {
    se[SummarizedExperiment::rowData(se)$std_type == "Quant", ]
  })
}

#' Calculate RSD of the quantification standard samples
#'
#' @param qc_se A list of SummarizedExperiment objects with quantitative standards in QC samples
#' @param assay_ids A character vector of the assay IDs
#' @return A tibble with RSD% of the quantification standard samples
#' @export
calc_rsd_qstd <- function(qc_se, assay_ids) {
  lapply(qc_se, \(qc1) {
    sapply(stats::setNames(nm = assay_ids), function(nm) {
      apply(SummarizedExperiment::assay(qc1, nm), 1, rsd_perc)
    }) |>    # Rows are chemicals, columns are assay IDs
      tibble::as_tibble(rownames = "chem_id")
  }) |> 
    # `QC` has the name of the QC samples
    dplyr::bind_rows(.id = "QC")
}
