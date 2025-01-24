#' Add intermediate data at each step of quality control
#'
#' @param name A character string for the name of the intermediate data
#' @param data The data to be stored with the `name`
#' @param file A file name to store the intermediate data
#' @export
append_to_qc_steps <- function(..., file) {
  if (!file.exists(file)) stop("Run `initialize_qc_steps` first.")
  qc_steps <- readRDS(file)
  dots <- list(...)
  nms <- names(dots)
  stopifnot(
    "Name of the intermediate data must be provided" = !any(sapply(nms, is.null)),
    "Duplicated names" = anyDuplicated(c(nms, names(qc_steps))) == 0
  )
  for(nm in nms) {
    qc_steps[[nm]] <- dots[[nm]]
  }
  saveRDS(qc_steps, file)
}
#' @rdname append_to_qc_steps
#' @export
initialize_qc_steps <- function(file) {
  saveRDS(list(), file)
}

# Clean ----------------------------------------------------------------------------------

#' Count zeros per feature
#'
#' @param mat A numeric matrix
#' @return A numeric vector of the number of zeros per feature in rows
#' @export
count_zeros_per_feature <- function(mat) {
  rowSums(mat == 0) |> 
    labelled::set_label_attribute("Number of zeros")
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

#' Count outlying internal standard features per sample 
#'
#' @param se A [`SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @return A numeric vector of the number of outlying internal standard features per sample
#' @export
count_outliers_per_sample <- function(se, mat_id, times = 3) {
  x <- se[[mat_id]]
  x <- log1p(x)              # Log-transform the data
  outlying <- apply(x, 1, identify_outliers, times = times)
  return(rowSums(outlying))     # Transposed by `apply` above
}



#' Get the values of the closest internal standard features
#'
#' @param se A [`SumExp`] object
#' @param istd_se A [`SumExp`] object of internal standard features
#' @param mat_id The name of a matrix in `se` 
#' @param rt The name of the retention time column 
#' @return A numeric matrix of the values of the closest internal standard features
#' @export
get_value_of_closest_istd <- function(se, istd_se, mat_id, rt = "rt") {
  rt_x <- SumExp::row_df(se)[[rt]]
  rt_istd <- SumExp::row_df(istd_se)[[rt]]
  # Find the closest internal standard feature
  i_closest <- sapply(rt_x, \(.x) which.min(abs(rt_istd - .x)))
  out <- istd_se[[mat_id]][i_closest, ]
  rownames(out) <- rownames(se)
  return(out)
}

# Normalization --------------------------------------------------------------------------

#' Get the LOESS fit model
#'
#' @param istd_se A [`SumExp`] object of internal standard features
#' @param excl_cat A character vector of the categories to exclude
#' @param overall_rt_range A numeric vector of the overall retention time range, to which the
#'   model is expanded
#' @param span A numeric value for the span of the LOESS fit
#' @param mat_id The name of a matrix in `istd_se`
#' @param rt The name of the retention time column
#'
#' @return A list of the LOESS fit models
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, mat_id, rt = "rt") {
  # Log-transform the data
  istd_log <- log(istd_se[[mat_id]])
  rt_istd <- SumExp::row_df(istd_se)[[rt]]
  
  # Normalize the data using the internal standards
  # Mean of each internal standard feature for overall measurement samples
  # , excluding the calibration curve and blank samples
  to_excl <- SumExp::col_df(istd_se)$contr_cat %in% excl_cat
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
  
  # LOESS fit of the internal standard features along RT
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

#' Subtract the average values of the blank samples from the samples
#'
#' @param x_se A [`SumExp`] object of the samples
#' @param condition_blank A condition to select the blank samples. Evaluated in the context of
#'   the columns of `x_se`
#' @param condition_no_change A condition to select the samples that should not be changed.
#' @param mat_ids The names of matrices in `x_se`, from which the blank values are subtracted
#' @param out_mat_ids The names of the output matrices in the returned object. These should be
#'   the same size as `mat_ids` in the same order.
#'
#' @return A [`SumExp`] object with the blank values subtracted. The columns of the blanks are
#'   removed from the `x_se`.
#' @export
subtract_blank_sumexp <- function(x_se, 
                                  condition_blank, 
                                  condition_no_change,
                                  mat_ids, 
                                  out_mat_ids = mat_ids) {
  stopifnot(length(mat_ids) == length(out_mat_ids))
  is_blank <- eval(substitute(condition_blank), SumExp::col_df(x_se), parent.frame())
  is_no_chg <- eval(substitute(condition_no_change), SumExp::col_df(x_se), parent.frame())
  blank_se <- x_se[, is_blank]
  x_se <- x_se[, !is_blank]
  is_no_chg <- is_no_chg[!is_blank]     # Used with updated `x_se`
  for(ii in seq(mat_ids)) {      # Paired `mat_ids` and `out_mat_ids`
    mat_id <- mat_ids[ii]
    x_mat <- x_se[[mat_id]]
    x_lab <- labelled::get_label_attribute(x_mat)
    blank_mean <- rowMeans(blank_se[[mat_id]])      # Mean per feature
    # Blank means are saved in the row_df of x_se with this name
    r_nm <- paste0(mat_id, "_blank_mean")
    SumExp::row_df(x_se)[[r_nm]] <- blank_mean |> 
      labelled::set_label_attribute(paste("Blank mean of", x_lab))
    mat_subt <- x_mat - blank_mean      # <<---- Subtract the blank average
    mat_subt[mat_subt < 0] <- 0
    mat_subt[, is_no_chg] <- x_mat[, is_no_chg]
    x_se[[ out_mat_ids[ii] ]] <- mat_subt |> 
      labelled::set_label_attribute(paste(x_lab, "(blank adjusted)"))
  }
  x_se
}



# Calibration ----------------------------------------------------------------------------

#' Identify the maximum concentration of the calibration samples
#'
#' @param cc_se A [`SumExp`] object of the calibration samples
#' @param q_se A [`SumExp`] object of the samples for measurement
#' @param mat_id The name of a matrix in `cc_se` and `q_se`
#' @param times A numeric value to multiply the maximum peak area of the samples for
#'   measurement to get a margin for the maximum concentration
#' @param concs A numeric vector of the concentrations of the calibration samples.
#'
#' @return A numeric vector of the maximum concentration of the calibration samples. 
#'   If all values of `q_se` are zero for a feature, the maximum concentration is zero. The
#'   names of the returned vector are the features. They are the same as the rownames of
#'   `cc_se`.
#' @export
identify_max_conc <- function(cc_se, q_se, mat_id, times, concs) {
  stopifnot(length(concs) == ncol(cc_se))
  stopifnot("Not identical features" = identical(rownames(q_se), rownames(cc_se)))
  q_se <- q_se[, quote(! contr_cat %in% c("CalCurve", "QC"))]
  # Find maximum peak area of the samples of the classes for measurement
  max_in_q_se <- apply(q_se[[mat_id]], 1, max, na.rm = TRUE)
  # Find the maximum concentration of the calibration samples that are not too far (`times`x)
  in_range <- cc_se[[mat_id]] <= times * max_in_q_se
  
  in_range <- cbind(in_range, all_0 = max_in_q_se == 0, no_valid = TRUE)
  # Add 0 and -9 to the end for all_0 and no_valid
  concs <- c(concs, 0, -9)
  out <- apply(in_range, 1, \(ea_feature) max(concs[ea_feature]))
  out <- labelled::set_label_attribute(out, "Maximum Concentration")
  stopifnot(identical(names(out), rownames(cc_se)))
  out
}

#' Make sure to have enough calibration curve samples
#'
#' @param max_c_conc A vector of the maximum concentration
#' @param concs A numeric vector of the concentrations of the calibration samples.
#' @param lloq A vector of the LLOQ
#' @param min_n Minimum number of concentrations
#' @param enough_n Enough number of concentrations
#'
#' @return A numeric vector of the maximum concentration of the calibration samples
#' @export
make_sure_to_have_enough_calcurve <- function(max_c_conc, concs, lloq, min_n = 3, enough_n = 5) {
  concs <- sort(unique(concs))
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
  stopifnot(identical(names(out), names(max_c_conc)))
  out
}

#' Get a list of a matrix of calibration curves by concentration
#'
#' @param cc_se A [`SumExp`] object of the calibration samples
#' @param mat_id The name of a matrix in `cc_se`
#' @param concs A numeric vector of the concentrations of the calibration samples.
#'
#' @return A list of matrices of the calibration curves split by concentration
.get_list_of_calcurve_by_conc <- function(cc_se, mat_id, concs) {
  stopifnot(length(concs) == ncol(cc_se))
  mat <- cc_se[[mat_id]]
  # Split the columns by the concentration
  sorted_conc <- stats::setNames(nm = sort(unique(concs)))
  lapply(sorted_conc, \(ea_c) mat[, concs == ea_c])
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
#' @param cc_0_se A [`SumExp`] object of the calibration samples with 0 concentration
#' @param mat_id The name of a matrix in `cc_0_se`
#' @param times A numeric value to multiply the mean of the samples with 0 concentration
#' @param na.rm A logical value to remove NA values
#' 
#' @return A numeric vector of the LLO(Q/D) in signal
#' @export
identify_llox_signal <- function(cc_0_se, mat_id, times, na.rm = FALSE) {
  mat <- cc_0_se[[mat_id]]
  m <- rowMeans(mat, na.rm = na.rm)
  m * times 
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @return A numeric value of RSD%
.rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Identify the lower limit of detection (LLOD) and quantification (LLOQ)
#'
#' @param cc_se A [`SumExp`] object of the calibration curve samples
#' @param llod_signal,lloq_signal A numeric vector of the LLOD/LLOQ in signal
#' @param mat_id The name of a matrix in `cc_se`
#' @param concs A numeric vector of the concentrations of the calibration samples.
#'
#' @return A numeric vector of the LLOD/LLOQ for each feature. The names are the same as the
#'   row names of `cc_se`.
#' @name llod_lloq
NULL
#' @rdname llod_lloq
#' @export
identify_llod <- function(cc_se, llod_signal, mat_id, concs) {
  cc_lst <- .get_list_of_calcurve_by_conc(cc_se, mat_id, concs)
  # Calculate the mean signal values of the calibration samples
  m <- sapply(cc_lst, rowMeans, na.rm = TRUE)
  # LLOD for each feature in spiked concentrations
  out <- apply(llod_signal <= m, 1, get_min_value_satisfy_cond, colnames(m))
  out <- labelled::set_label_attribute(out, "LLOD")
  stopifnot(identical(names(out), rownames(cc_se)))
  out
}
#' @rdname llod_lloq
#' @export
identify_lloq <- function(cc_se, lloq_signal, mat_id, concs) {
  cc_lst <- .get_list_of_calcurve_by_conc(cc_se, mat_id, concs)
  # Calculate the mean signal values of the calibration samples
  m <- sapply(cc_lst, rowMeans, na.rm = TRUE)
  # LLOQ for each feature in spiked concentrations
  lloq <- apply(lloq_signal <= m, 1, get_min_value_satisfy_cond, colnames(m))
  
  # Find minimum concentration that pass the condition RSD% < 20%
  rsdp <- sapply(cc_lst, apply, 1, .rsd_perc, na.rm = TRUE)
  rsdp <- rsdp < 20
  rsdp[m == 0] <- FALSE
  # Minimum concentration that satisfies the condition
  min_conc_by_rsdp <- apply(rsdp, 1, get_min_value_satisfy_cond, colnames(rsdp))
  
  out <- apply(cbind(lloq, min_conc_by_rsdp), 1, max)
  out <- labelled::set_label_attribute(out, "LLOQ")
  stopifnot(identical(names(out), rownames(cc_se)))
  out
}

#' Identify the limits in the calibration curve
#' 
#' LLOQ, LLOD, and maximum concentration
#' 
#' @param cc_se A [`SumExp`] object of the calibration curve samples
#' @param cc_0_se A [`SumExp`] object of the zero concentration samples
#' @param quant_se A [`SumExp`] object of the quantification samples
#' @param mat_id The name of a matrix in `cc_se`, `cc_0_se`, and `quant_se` to use
#' @param concs A vector of concentrations of the calibration curve samples
#' 
#' @return A data frame with the limits. The row names are the names of the calibration curve
#'   samples.
#' @export
identify_limts_in_calibrations <- function(cc_se, cc_0_se, quant_se, mat_id, concs) {
  # Find the lower-limit of quantification using values of the zero concentration samples
  lloq_signal <- identify_llox_signal(cc_0_se, mat_id, times = 10)
  lloq <- identify_lloq(cc_se, lloq_signal, mat_id, concs)
  # Find the lower-limit of detection using values of the zero concentration samples
  llod_signal <- identify_llox_signal(cc_0_se, mat_id, times = 3)
  llod <- identify_llod(cc_se, llod_signal, mat_id, concs)
  
  # Calibration curve concentration upper limit
  max_c_conc <- identify_max_conc(cc_se, quant_se, mat_id, times = 10, concs) |> 
    make_sure_to_have_enough_calcurve(concs, lloq = lloq, min_n = 3, enough_n = 5) 
  
  out <- data.frame(lloq, llod, max_c_conc, lloq_signal, llod_signal)
  stopifnot(identical(rownames(out), rownames(cc_se)))
  out
}

#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param value A vector of signal values
#'
#' @return A list with the best model, the name of it, the R2 values of all models, and the
#'   number of unique concentrations
#' @examples
#' conc <- rep(c(0.1, 0.2, 0.5, 1, 2), 3)
#' value <- conc * 5 + rnorm(length(conc))
#' fit_and_test_calcurve_model(conc, value)
#' @name calcurve_model
NULL
#' @rdname calcurve_model
#' @export
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
  R2s <- sapply(models, \(x) summary(x$inv_mod)$r.squared)
  rlang::list2(
    "best_model" = models[[which.max(R2s)]]$model,        # Best model by R2
    "best_model_name" = names(models)[which.max(R2s)],
    "R2s" = R2s,
    "n_conc" = length(unique(conc[!is.na(value)])),      # Number of unique concentrations
    "inv_model" = models[[which.max(R2s)]]$inv_mod,
  )
}
#' @param weights weights for linear model fit
#' @rdname calcurve_model
quadratic_calcurve_model <- function(conc, value, weights) {
  lmfit <- stats::lm(value ~ conc + I(conc^2), weights = weights)
  beta <- stats::coef(lmfit)
  a <- beta[["I(conc^2)"]]
  b <- beta[["conc"]]
  cc <- beta[["(Intercept)"]]
  model <- function(x) {
    det <- b ^ 2 - 4 * a * (cc - x)
    det <- ifelse(det < 0, 0, det)       # det < 0
    conc <- (-b + sqrt(det)) / (2 * a)
    conc[conc < 0] <- 0
    conc
  }
  list(model = model, inv_mod = lmfit)
}
#' @rdname calcurve_model
linear_calcurve_model <- function(conc, value, weights) {
  lmfit <- stats::lm(value ~ conc, weights = weights)
  beta <- stats::coef(lmfit)
  b1 <- beta[["conc"]]
  b0 <- beta[["(Intercept)"]]
  model <- function(x) {
    (x - b0) / b1
  }
  list(model = model, inv_mod = lmfit)
}

#' Compute the concentration of features
#'
#' @param x_se A [`SumExp`] object of the samples
#' @param cc_se A [`SumExp`] object of the calibration samples
#' @param calcurve_models A list of calibration curve models
#' @param mat_id The name of a matrix in `x_se`
#'
#' @return A matrix of the concentration of features
#' @export
compute_concentration <- function(x_se, cc_se, calcurve_models, mat_id) {
  mat <- x_se[[mat_id]]
  # Calculate the concentration of each feature
  conc <- sapply(rownames(x_se), \(i_feature) {
    v <- mat[i_feature, ]
    # Concentration by the best model
    calcurve_models[[i_feature]]$best_model(v)
  }) |> 
    t()          # Features to rows
  conc <- labelled::set_label_attribute(conc, "Concentration")
  conc
}



#' Replace the values below the LLOQ and LLOD with half of the LLOQ and 1/4 of the LLOQ
#' 
#' @param conc A matrix of concentrations
#' @param limits A list with the LLOQ and LLOD values and signal values. The required columns
#'   are `lloq` and `llod`
#' 
#' @return A matrix with the values below the LLOQ and LLOD replaced
#' @export
replace_below_lloq_llod <- function(conc, limits) {
  lab <- labelled::get_label_attribute(conc)
  # Replace the values below LLOQ with half of the LLOQ
  conc <- ifelse(conc < limits$lloq, limits$lloq / 2, conc)
  # Replace the values below LLOD with 1/4 of the LLOQ
  conc <- ifelse(conc < limits$llod, limits$lloq / 4, conc)
  conc <- labelled::set_label_attribute(conc, lab)
  conc
}


#' Replace the values outside the concentration range with NA
#'
#' @param cc_mat A matrix of the calibration curve values
#' @param concs A vector of concentrations of the calibration curve samples
#' @param min_conc,max_conc A vector of minimum and maximum concentrations
#'
#' @return A matrix with the values outside the concentration range replaced with NA
#' @export
replace_outside_concentration_range_with_na <- function(cc_mat, concs, min_conc, max_conc) {
  stopifnot(
    length(concs) == ncol(cc_mat),
    length(min_conc) == nrow(cc_mat),
    length(max_conc) == nrow(cc_mat)
  )
  # Concentration values in a matrix, rows are features and columns are concentrations
  conc_mat <- matrix(rep(concs, each = nrow(cc_mat)), nrow = nrow(cc_mat))
  to_na <- conc_mat < min_conc | conc_mat > max_conc
  cc_mat[to_na] <- NA_real_
  cc_mat
}


