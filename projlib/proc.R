#' Extract QC samples into list
#'
#' @param se A SumExp object
#'
#' @return A list of SumExp objects divided by QC Classes
#' @export
extract_qc_samples_to_list <- function(se) {
  se <- se[, SumExp::col_df(se)$proc_cat == "QC"]
  qc_id <- SumExp::col_df(se)$Class
  # Column-wise split
  lapply(stats::setNames(nm = unique(qc_id)), \(ii) se[, qc_id == ii])
}

#' Extract quantitative standards in QC samples
#'
#' @param sumexp A SumExp object
#' @return A list of SumExp objects with quantitative standards in QC samples
#' @export
extract_quant_qc <- function(sumexp) {
  se_lst <- extract_qc_samples_to_list(sumexp)
  lapply(se_lst, \(se) {
    se[SumExp::row_df(se)$std_type == "Quant", ]
  })
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @return A numeric value of RSD%
#' @export
rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}


# Clean ----------------------------------------------------------------------------------

#' Get the internal standard chemicals from a SumExp object
#'
#' @param se A SumExp object
#'
#' @return A SumExp object with the internal standard chemicals
#' @export
get_internal_std_se <- function(se) {
  # Extract the internal standard chemicals
  se <- se[SumExp::row_df(se)$std_type == "IS", ]
  # Sort by average retention time
  se <- se[order(SumExp::row_df(se)$rt), ]
  return(se)
}

#' Count zeros per chemical
#'
#' @param se A SumExp object 
#' @param assay_id The name of an assay 
#' @return A numeric vector of the number of zeros per chemical
#' @export
count_zeros_per_chemical <- function(se, assay_id = "raw") {
  rowSums(SumExp::assay(se, assay_id) == 0)
}

#' RSD% across all samples
#'
#' @param se A SumExp object 
#' @param assay_id The name of an assay
#' @return A numeric vector of RSD% across all samples
#' @export
compute_rsd_per_chemical <- function(se, assay_id = "raw") {
  apply(SumExp::assay(se, assay_id), 1, rsd_perc)
}

#' Add the number of zeros and RSD% to the row_df of a SumExp object
#'
#' @inheritParams count_zeros_per_chemical
#' @return A SumExp object with `num_zeros` and `rsd` added to the row_df
#' @export
add_num_zeros_rsd <- function(se, assay_id = "raw") {
  SumExp::row_df(se)$num_zeros <- count_zeros_per_chemical(se, assay_id) |> 
    labelled::set_label_attribute("Number of zeros per chemical")
  SumExp::row_df(se)$rsd <- compute_rsd_per_chemical(se, assay_id) |> 
    labelled::set_label_attribute("RSD% across all samples")
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
#' @param se A SumExp object
#' @param assay_id The name of an assay
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @return A numeric vector of the number of outlying internal standard chemicals per sample
#' @export
count_outliers_per_sample <- function(se, assay_id = "raw", times = 3) {
  x <- SumExp::assay(se, assay_id)
  x <- log1p(x)              # Log-transform the data
  outlying <- apply(x, 1, identify_outliers, times = times)
  return(rowSums(outlying))     # Transposed by `apply` above
}



#' Get the values of the closest internal standard chemicals
#'
#' @param se A SumExp object
#' @param istd_se A SumExp object of internal standard chemicals
#' @param assay_id The name of an assay 
#' @param rt The name of the retention time column 
#' @return A numeric matrix of the values of the closest internal standard chemicals
#' @export
get_raw_of_closest_istd <- function(se, istd_se, assay_id = "raw", rt = "rt") {
  rt_x <- SumExp::row_df(se)[[rt]]
  rt_istd <- SumExp::row_df(istd_se)[[rt]]
  # Find the closest internal standard chemical
  i_closest <- sapply(rt_x, \(.x) which.min(abs(rt_istd - .x)))
  out <- SumExp::assay(istd_se, assay_id)[i_closest, ]
  rownames(out) <- rownames(se)
  return(out)
}

# Normalization --------------------------------------------------------------------------

#' Get the LOESS fit model
#'
#' @param istd_se A SumExp object of internal standard chemicals
#' @param excl_cat A character vector of the categories to exclude
#' @param overall_rt_range A numeric vector of the overall retention time range, to which the model is expanded
#' @param span A numeric value for the span of the LOESS fit
#' @param assay_id The name of an assay
#' @param rt The name of the retention time column
#'
#' @return A list of the LOESS fit models
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, assay_id = "raw", rt = "rt") {
  # Log-transform the data
  istd_log <- log(SumExp::assay(istd_se, assay_id))
  rt_istd <- SumExp::row_df(istd_se)[[rt]]
  
  # Normalize the data using the internal standards
  # Mean of each internal standard chemical for overall measurement samples
  # , excluding the calibration curve and blank samples
  to_excl <- SumExp::col_df(istd_se)$proc_cat %in% excl_cat
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
  loess_fit
}

#' Calculate RSD of the quantification standard samples
#'
#' @param qc_se A list of SumExp objects with quantitative standards in QC samples
#' @param assay_ids A character vector of the assay IDs
#' @return A tibble with RSD% of the quantification standard samples
#' @export
calc_rsd_qstd <- function(qc_se, assay_ids) {
  lapply(qc_se, \(qc1) {
    sapply(stats::setNames(nm = assay_ids), function(nm) {
      apply(SumExp::assay(qc1, nm), 1, rsd_perc)
    }) |>    # Rows are chemicals, columns are assay IDs
      tibble::as_tibble(rownames = "chem_id")
  }) |> 
    # `QC` has the name of the QC samples
    dplyr::bind_rows(.id = "QC")
}


# Calibration ----------------------------------------------------------------------------

#' Identify the maximum concentration of the calibration samples
#'
#' @param cc_se A SumExp object of the calibration samples
#' @param q_se A SumExp object of the samples for measurement
#' @param assay_id The name of an assay
#' @param times A numeric value to multiply the maximum peak area of the samples for
#'   measurement to get a margin for the maximum concentration
#'
#' @return A numeric vector of the maximum concentration of the calibration samples. 
#'   If all values of `q_se` are zero for a chemical, the maximum concentration is zero.
#' @export
identify_max_conc <- function(cc_se, q_se, assay_id, times) {
  stopifnot("Not identical chemicals" = identical(rownames(q_se), rownames(cc_se)))
  q_se <- q_se[, ! SumExp::col_df(q_se)$proc_cat %in% c("CalCurve", "QC")]
  # Find maximum peak area of the samples of the classes for measurement
  max_in_q_se <- apply(SumExp::assay(q_se, assay_id), 1, max, na.rm = TRUE)
  # Find the maximum concentration of the calibration samples that are not too far (`times`x)
  in_range <- SumExp::assay(cc_se, assay_id) <= times * max_in_q_se
  
  in_range <- cbind(in_range, all_0 = max_in_q_se == 0, no_valid = TRUE)
  # Add 0 and -9 to the end for all_0 and no_valid
  concs <- c(SumExp::col_df(cc_se)$c_conc, 0, -9)
  apply(in_range, 1, \(ea_chem) max(concs[ea_chem])) |> 
    labelled::set_label_attribute("Maximum Concentration")
}

#' Make sure to have enough calibration curve samples
#'
#' @param concs A numeric vector of the concentrations of the calibration samples. Must be sorted
#' @param lloq A vector of the LLOQ
#' @param max_c_conc A vector of the maximum concentration
#' @param min_n Minimum number of concentrations
#' @param enough_n Enough number of concentrations
#'
#' @return A numeric vector of the maximum concentration of the calibration samples
#' @export
make_sure_to_have_enough_calcurve <- function(concs, lloq, max_c_conc, min_n = 3, enough_n = 5) {
  stopifnot(identical(sort(concs), concs))   # Already sorted 
  # Find the index of the LLOQ
  i_lloq <- match(lloq, concs)
  # Find the index of the maximum concentration
  i_max <- match(max_c_conc, c(0, concs)) - 1   # If `max_c_conc` == 0, take the largest range
  # Find the index of the maximum concentration that provide enough number of concentrations
  i_max <- ifelse(i_lloq + enough_n - 1 <= i_max, i_max, i_lloq + enough_n - 1)
  # Limit to the length of the concentrations
  i_max <- ifelse(i_max > length(concs), length(concs), i_max)
  # If the maximum concentration is too close to the LLOQ, return NA
  i_max <- ifelse(i_max >= i_lloq + min_n - 1, i_max, NA_real_)
  out <- concs[i_max]
  names(out) <- names(max_c_conc)
  out <- labelled::copy_labels(max_c_conc, out)
  out
}

#' Get a list of an assay data of calibration curves by concentration
#'
#' @param cc_se A SumExp object of the calibration samples
#' @param assay_id The name of an assay
#'
#' @return A list of matrices of the assay data of calibration curves split by concentration
#' @export
get_list_of_calcurve_by_conc <- function(cc_se, assay_id) {
  mat <- SumExp::assay(cc_se, assay_id)
  conc <- SumExp::col_df(cc_se)$c_conc
  # Split the columns by the concentration
  sorted_conc <- stats::setNames(nm = sort(unique(conc)))
  lapply(sorted_conc, \(ea_c) mat[, conc == ea_c])
}
#' Get the minimum value that satisfies the condition
get_min_value_satisfy_cond <- function(cond, values, shift = 0) {
  values <- as.numeric(values)
  stopifnot(identical(sort(values), values))   # Already sorted 
  if (all(!cond)) return(NA_real_)
  ii <- min(which(cond))
  values[ii + shift]
}

#' Identify the LLO(Q/D) in signal
#'
#' @param cc_0_se A SumExp object of the calibration samples with 0 concentration
#' @param assay_id The name of an assay
#' @param times A numeric value to multiply the mean of the samples with 0 concentration
#' @param na.rm A logical value to remove NA values
#' 
#' @return A numeric vector of the LLO(Q/D) in signal
#' @export
identify_llox_signal <- function(cc_0_se, assay_id, times, na.rm = FALSE) {
  mat <- SumExp::assay(cc_0_se, assay_id)
  m <- rowMeans(mat, na.rm = na.rm)
  m * times 
}

#' Identify the lower limit of detection (LLOD)
#'
#' @param cc_se A SumExp object of the calibration samples
#' @param llod_signal A numeric vector of the LLOD in signal
#' @param assay_id The name of an assay
#'
#' @return A numeric vector of the LLOD for each chemcial
#' @export
identify_llod <- function(cc_se, llod_signal, assay_id) {
  cc_lst <- get_list_of_calcurve_by_conc(cc_se, assay_id)
  # Calculate the mean signal values of the calibration samples
  m <- sapply(cc_lst, rowMeans, na.rm = TRUE)
  # LLOD for each chemical in spiked concentrations
  out <- apply(llod_signal <= m, 1, get_min_value_satisfy_cond, colnames(m))
  out <- labelled::set_label_attribute(out, "LLOD")
  out
}

#' Identify the lower limit of quantification (LLOQ)
#'
#' @param cc_se A SumExp object of the calibration samples
#' @param lloq_signal A numeric vector of the LLOQ in signal
#' @param assay_id The name of an assay
#'
#' @return A numeric vector of the LLOQ for each chemical
#' @export
identify_lloq <- function(cc_se, lloq_signal, assay_id) {
  cc_lst <- get_list_of_calcurve_by_conc(cc_se, assay_id)
  # Calculate the mean signal values of the calibration samples
  m <- sapply(cc_lst, rowMeans, na.rm = TRUE)
  # LLOQ for each chemical in spiked concentrations
  lloq <- apply(lloq_signal <= m, 1, get_min_value_satisfy_cond, colnames(m))
  
  # Find minimum concentration that pass the condition RSD% < 20%
  rsdp <- sapply(cc_lst, apply, 1, rsd_perc, na.rm = TRUE)
  rsdp <- rsdp < 20
  rsdp[m == 0] <- FALSE
  # Minimum concentration that satisfies the condition
  min_conc_by_rsdp <- apply(rsdp, 1, get_min_value_satisfy_cond, colnames(rsdp))
  
  out <- apply(cbind(lloq, min_conc_by_rsdp), 1, max)
  out <- labelled::set_label_attribute(out, "LLOQ")
  out
}

#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param value A vector of signal values
#'
#' @return
#' @export
#' @rdname calcurve_model
fit_and_test_calcurve_model <- function(conc, value) {
  if (all(is.na(value) | value == 0)) {
    return(list(
      "best_model" = function(x) x,
      "R2s" = NA,
      "best_model_name" = NA_character_
    ))
  }
  # Weight alternatives
  weights_alt <- rlang::list2(
    "1" = rep(1, length(conc)),
    "1/x" = 1 / conc,
    "1/x2" = 1 / (conc ^ 2),
  )
  # Linear and quadratic models
  lmodels <- lapply(weights_alt, \(w) linear_calcurve_model(conc, value, weights = w))
  names(lmodels) <- paste("linear", names(lmodels), sep = "-")
  qmodels <- lapply(weights_alt, \(w) quadratic_calcurve_model(conc, value, weights = w))
  names(qmodels) <- paste("quadratic", names(qmodels), sep = "-")
  models <- c(lmodels, qmodels)
  R2s <- sapply(models, \(f) attr(f, "R2"))
  list(
    "best_model" = models[[which.max(R2s)]],        # Best model by R2
    "R2s" = R2s,
    "best_model_name" = names(models)[which.max(R2s)]
  )
}
#' @rdname calcurve_model
quadratic_calcurve_model <- function(conc, value, weights) {
  lmfit <- stats::lm(value ~ conc + I(conc^2), weights = weights)
  beta <- stats::coef(lmfit)
  a <- beta[["I(conc^2)"]]
  b <- beta[["conc"]]
  cc <- beta[["(Intercept)"]]
  out <- function(x) {
    det <- b ^ 2 - 4 * a * (cc - x)
    conc <- suppressWarnings((-b + sqrt(det)) / (2 * a))      # det < 0
    conc[conc < 0 | conc < 0] <- 0
    conc
  }
  attr(out, "R2") <- summary(lmfit)$r.squared
  out
}
#' @rdname calcurve_model
linear_calcurve_model <- function(conc, value, weights) {
  lmfit <- stats::lm(value ~ conc, weights = weights)
  beta <- stats::coef(lmfit)
  b1 <- beta[["conc"]]
  b0 <- beta[["(Intercept)"]]
  out <- function(x) {
    (x - b0) / b1
  }
  attr(out, "R2") <- summary(lmfit)$r.squared
  out
}

#' Compute the concentration of chemicals
#'
#' @param x_se A SumExp object of the samples
#' @param cc_se A SumExp object of the calibration samples
#' @param calcurve_models A list of calibration curve models
#' @param assay_id The name of an assay
#'
#' @return A matrix of the concentration of chemicals
#' @export
compute_concentration <- function(x_se, cc_se, calcurve_models, assay_id) {
  mat <- SumExp::assay(x_se, assay_id)
  # Calculate the concentration of each chemical
  conc <- sapply(rownames(x_se), \(i_chem) {
    v <- mat[i_chem, ]
    # Concentration by the best model
    calcurve_models[[i_chem]]$best_model(v)
  }) |> 
    t()          # Chemicals to rows
  
  # Replace the values below LLOQ with half of the LLOQ
  rl <- SumExp::row_df(cc_se)
  conc <- ifelse(mat < rl$lloq_signal, rl$lloq / 2, conc)
  # Replace the values below LLOD with 1/4 of the LLOQ
  conc <- ifelse(mat < rl$llod_signal, rl$lloq / 4, conc)
  conc <- labelled::set_label_attribute(conc, "Concentration")
  return(conc)
}







