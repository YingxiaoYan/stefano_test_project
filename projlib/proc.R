#' Add intermediate data at each step of quality control
#'
#' @param name A character string for the name of the intermediate data
#' @param data The data to be stored with the `name`
#' @param file A file name to store the intermediate data
#' @export
append_to_qc_steps <- function(..., file) {
  if (!file.exists(file)) stop("Run `initialize_qc_steps` first.")
  qc_steps <- readRDS(file)
  dots <- rlang::list2(...)
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
# Normalization using internal standards -------------------------------------------------

## Volumetric normalization -------------------

#' Normalize the data by the volumetric internal standard
#'
#' @param se A [`SumExp`] object
#' @param is_vIS A logical vector indicating the volumetric internal standard. Only one 
#'   volumetric internal standard is allowed. The length of the vector should be the same as
#'   the number of rows of `se`.
#' @param mat_id The name of a matrix in `se` to be normalized
#' @returns A [`SumExp`] object with the volumetric normalization. The normalized matrix is
#'   added to the `se` with the name `vol_norm`.
#' @export
normalize_volumetric <- function(se, is_vIS, mat_id) {
  stopifnot("Only one volumetric internal standard is allowed." = sum(is_vIS) == 1)
  stopifnot(nrow(se) == length(is_vIS))
  vIS_se <- se[is_vIS, ]
  se <- se[!is_vIS, ]

  v <- as.vector(vIS_se[[mat_id]])
  mat <- t(replicate(nrow(se), v))  # Column-wise normalization
  # <<---- Volumetric normalization ---->> #
  se[["vol_norm"]] <- se[[mat_id]] / mat * mean(v, na.rm = TRUE)
  labelled::label_attribute(se[["vol_norm"]]) <- "Volumetric normalized peak area"
  se
}

## Clean ------------------

#' Count zeros per feature
#'
#' @param mat A numeric matrix
#' @returns A numeric vector of the number of zeros per feature in rows
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
#' @returns A logical vector indicating whether each element is an outlier
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
#' @returns A numeric vector of the number of outlying internal standard features per sample
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
#' @returns A numeric matrix of the values of the closest internal standard features
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

## Normalization models ------------------

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
#' @returns A list of the LOESS fit models
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

## Blank subtraction ------------------

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
#' @returns A [`SumExp`] object with the blank values subtracted. The columns of the blanks are
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

#' Split a matrix column-wise by the values
#'
#' @param x A matrix or a data frame to be split
#' @param value A numeric vector of the values to split the matrix. The length of the vector
#'   should be the same as the number of columns of `x`.
#'
#' @returns A list of matrices split by the values column-wise. The list is sorted by the
#'   values.
#' @export
split_column_and_sort_by <- function(x, value) {
  stopifnot(length(value) == ncol(x))
  # Split the columns by the values
  sorted_v <- stats::setNames(nm = sort(unique(value)))
  lapply(sorted_v, \(ea_c) x[, value == ea_c, drop = FALSE])
}

#' Get the minimum value that satisfies the condition
#' 
#' @param cond A logical value to satisfy.
#' @param values A numeric or character vector of values that are already sorted. If it is not
#'   given, the names of `cond` are used. When it is a character vector, the values are
#'   converted to numeric.
#' @param shift A numeric value to shift the index
#' 
#' @returns A numeric value from the `values` that satisfies the condition `cond`. If no value
#'   satisfies the condition, return `NA_real_`.
#' @examples
#' get_min_value_satisfy_cond(c(TRUE, FALSE, TRUE), c(5:7))
#' get_min_value_satisfy_cond(c("4" = FALSE, "5" = TRUE, "6" = TRUE))
#' get_min_value_satisfy_cond(c(1:3) > 1, c(5:7)) 
#' \dontrun{
#' get_min_value_satisfy_cond(c(1:3) > 1, c(5, 3, 9))        # Error because not sorted
#' }
get_min_value_satisfy_cond <- function(cond, values = names(cond), shift = 0) {
  values <- as.numeric(values)
  stopifnot("`values` are not sorted" = identical(sort(values), values))   # Already sorted 
  if (all(!cond)) return(NA_real_)
  ii <- min(which(cond))
  values[ii + shift]
}

#' Identify the minimum concentration with non-zero mean
#'
#' @param cc_lst A list of matrices of the concentration values. The concentration values are
#'   supposed to be sorted. Each matrix should have the same number of rows. 
#' @returns A numeric vector of the minimum concentration values that have non-zero mean.
identify_min_conc_with_non_zero_mean <- function(cc_lst) {
  # Calculate the mean values per concentration
  m <- sapply(cc_lst, rowMeans, na.rm = TRUE)
  apply(m > 0, 1, get_min_value_satisfy_cond)
}

#' Identify the minimum concentration having significant difference from the first non-zero concentration
idenfity_min_signif_diff_conc <- function(cc_lst, non_zero) {
  stopifnot(length(non_zero) == nrow(cc_lst[[1]]))
  n_nz <- length(non_zero)
  cc_str <- names(cc_lst)    # Concentration values in string
  nz_str <- as.character(non_zero)     # To search in a list
  sapply(stats::setNames(1:n_nz, nm = names(non_zero)), \(i) {    # Index of each row
    # Values of the first non-zero concentration
    nz_v <- cc_lst[[nz_str[i]]][i, ]
    cc_gt_nz <- cc_str[as.numeric(cc_str) > non_zero[i]]   # Only search greater concentrations
    for(cc_i in cc_gt_nz) {        # To return whenever satisfies p < 0.05
      # Values of other concentration
      gt_nz_v <- cc_lst[[cc_i]][i, ]
      p <- stats::t.test(nz_v, gt_nz_v)$p.value
      if (p < 0.05) return(as.numeric(cc_i))
    }
    return(NA_real_)      # If no concentration is significantly different
  })
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @returns A numeric value of RSD%
.rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Identify the minimum concentration point for calibration curve
#' 
#' It satisfies the following conditions:
#' 1. The minimum concentration point having significant difference from the non-zero
#'   concentration given by `non_zero_conc`.
#' 2. The minimum concentration point that pass the condition RSD% < 20%
#'
#' @param cc_lst A list of matrices of the concentration values. The concentration values are
#'   supposed to be sorted. Each matrix should have the same number of rows. 
#' @param non_zero_conc A numeric vector of the non-zero concentration values. The length of 
#'   the vector should be the same as the number of rows of the matrices in `cc_lst`.
#'
#' @returns A numeric vector of the minimum concentrations that satisfy the conditions.
identify_min_conc_for_curve <- function(cc_lst, non_zero_conc) {
  # The minimum concentration having significant difference from the non-zero concentration
  sg_diff_conc <- idenfity_min_signif_diff_conc(cc_lst, non_zero_conc)
  # Find minimum concentration that pass the condition RSD% < 20%
  rsdp <- sapply(cc_lst, apply, 1, .rsd_perc, na.rm = TRUE)
  c_by_rsdp <- (!is.na(rsdp) & rsdp < 20) |> 
    apply(1, get_min_value_satisfy_cond, colnames(rsdp))
  # Satisfies the two conditions, significant difference and RSD% < 20%
  out <- ifelse(sg_diff_conc > c_by_rsdp, sg_diff_conc, c_by_rsdp)
  out <- labelled::set_label_attribute(out, "Minimum Concentration")
  out
}

#' Identify the maximum concentration point of the calibration samples
#'
#' @param cc_se A [`SumExp`] object of the calibration samples
#' @param q_se A [`SumExp`] object of the samples to be calibrated
#' @param mat_id The name of a matrix in `cc_se` and `q_se`
#' @param times A numeric value to multiply the maximum peak area of the samples for
#'   measurement to get a margin for the maximum concentration
#' @param conc A numeric vector of the concentrations of the calibration samples.
#'
#' @returns A numeric vector of the maximum concentration of the calibration samples. 
#'   If all values of `q_se` are zero for a feature, the maximum concentration is zero. The
#'   names of the returned vector are the features. They are the same as the rownames of
#'   `cc_se`.
identify_max_conc_for_curve <- function(cc_se, q_se, mat_id, times, conc) {
  stopifnot(length(conc) == ncol(cc_se))
  stopifnot("Not identical features" = identical(rownames(q_se), rownames(cc_se)))
  # Find maximum peak area of the samples of the classes for measurement
  max_in_q_se <- apply(q_se[[mat_id]], 1, max, na.rm = TRUE)
  # Find the maximum concentration of the calibration samples that are not too far (`times`x)
  in_range <- cc_se[[mat_id]] <= times * max_in_q_se
  
  # `all_0` = if all values of `q_se` are zero for a feature
  in_range <- cbind(in_range, all_0 = max_in_q_se == 0, no_valid = TRUE)
  # Add 0 and -9 to the end for all_0 and no_valid
  conc <- c(conc, 0, -9)
  out <- apply(in_range, 1, \(ea_feature) max(conc[ea_feature]))
  out <- labelled::set_label_attribute(out, "Maximum Concentration")
  stopifnot(identical(names(out), rownames(cc_se)))
  out
}

#' Make sure to have enough calibration curve samples
#'
#' @param max_conc A vector of the maximum concentration
#' @param min_conc A vector of the minimum concentration
#' @param conc A numeric vector of the concentrations of the calibration samples.
#' @param min_n Required minimum number of concentrations for the calibration curve. If the 
#'   number of concentrations is less than this, `NA` is returned.
#' @param enough_n Enough number of concentrations for the calibration curve
#'
#' @returns A numeric vector of the maximum concentrations that provide enough number of spiked
#'   concentrations. The names are the same as the input vectors `max_conc`. If the number of
#'   concentrations is less than `min_n`, the maximum concentration is set to `NA`
make_sure_to_have_enough_calcurve <- function(max_conc, min_conc, conc, min_n = 3, enough_n = 5) {
  uniq_conc <- sort(unique(conc))
  n_uniq_conc <- length(uniq_conc)
  i_min <- match(min_conc, uniq_conc)     # Index of the minimum concentration
  # Find the index of the maximum that provides enough number of spiked concentrations
  i_max <- match(max_conc, c(0, uniq_conc)) - 1   # If `max_conc` == 0, take the largest range
  i_enough_from_min <- i_min + enough_n - 1   # Counting from minimum
  i_max <- ifelse(i_enough_from_min <= i_max, i_max, i_enough_from_min)
  # Limit when `i_enough_from_min` > number of concentrations
  i_max <- ifelse(i_max > n_uniq_conc, n_uniq_conc, i_max)
  # If the number of valid concentrations is less than `min_n`, return NA
  i_max <- ifelse(i_max >= i_min + min_n - 1, i_max, NA_real_)
  out <- uniq_conc[i_max]
  names(out) <- names(max_conc)
  out <- labelled::copy_labels(max_conc, out)
  stopifnot(identical(names(out), names(max_conc)))
  out
}

#' Identify the limits in the calibration curve
#'
#' @param cc_se A [`SumExp`] object of the calibration curve samples
#' @param quant_se A [`SumExp`] object of the quantification samples
#' @param mat_id The name of a matrix in `cc_se`, `cc_0_se`, and `quant_se` to use
#' @param conc A vector of concentrations of the calibration curve samples
#'
#' @returns A data frame with the limits. The row names are the names of the calibration curve
#'   samples. The columns are `non_zero_conc`, `min_c_conc` and `max_c_conc` for the lowest
#'   non-zero concentration, the lower and upper limits of the calibration curve, respectively.
#'   Some values in the columns are set to have the following meanings:
#'   min_c_conc = NA : No valid concentration even at the top of the calibration curve
#'   max_c_conc = 0 : All concentrations are zero
#'   max_c_conc = -9 : No valid maximum concentration
#'   max_c_conc = NA : Not enough calibration curve samples to make a calibration curve
#' @export
identify_limts_in_calibrations <- function(cc_se, quant_se, mat_id, conc) {
  mat <- cc_se[[mat_id]]
  # Split the matrix by the concentrations
  cc_lst <- split_column_and_sort_by(mat, conc)
  # The minimum concentration with non-zero mean
  non_zero_conc <- identify_min_conc_with_non_zero_mean(cc_lst)
  # Calibration curve concentration lower limit
  min_c_conc <- identify_min_conc_for_curve(cc_lst, non_zero_conc)
  # Calibration curve concentration upper limit
  quant_se <- quant_se[, quote(! contr_cat %in% c("QC"))]
  max_c_conc <- identify_max_conc_for_curve(cc_se, quant_se, mat_id, times = 10, conc) |>
    make_sure_to_have_enough_calcurve(min_c_conc, conc, min_n = 3, enough_n = 5)
  
  stopifnot(identical(names(min_c_conc), names(max_c_conc)))
  out <- data.frame(non_zero_conc, min_c_conc, max_c_conc)
  stopifnot(identical(rownames(out), rownames(cc_se)))
  out
}

#' Replace the values outside the concentration range with NA
#'
#' @param cc_mat A matrix of the calibration curve values
#' @param conc A vector of concentrations of the calibration curve samples
#' @param min_conc,max_conc A vector of minimum and maximum concentrations
#'
#' @returns A matrix with the values outside the concentration range replaced with NA
#' @export
replace_outside_concentration_range_with_na <- function(cc_mat, conc, min_conc, max_conc) {
  stopifnot(
    length(conc) == ncol(cc_mat),
    length(min_conc) == nrow(cc_mat),
    length(max_conc) == nrow(cc_mat)
  )
  # Concentration values in a matrix, rows are features and columns are concentrations
  conc_mat <- matrix(rep(conc, each = nrow(cc_mat)), nrow = nrow(cc_mat))
  to_na <- conc_mat < min_conc | conc_mat > max_conc
  cc_mat[to_na] <- NA_real_
  cc_mat
}


#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param value A vector of signal values
#' @param penalty_quadratic The penalty for the quadratic models
#'
#' @returns A list with the best model, the name of it, the R2 values of all models, and the
#'   number of unique concentrations. The best model is chosen as the model with the highest R2
#'   after applying the penalty to quadratic models. 
#' @examples
#' conc <- rep(c(0.1, 0.2, 0.5, 1, 2), 3)
#' value <- conc * 5 + rnorm(length(conc))
#' fit_and_test_calcurve_model(conc, value)
#' @name calcurve_model
NULL
#' @rdname calcurve_model
#' @export
fit_and_test_calcurve_model <- function(conc, value, penalty_quadratic = 0) {
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
  # Panelty to quadratic models
  R2s_adj <- R2s
  is_quadratic <- grepl("quadratic", names(models))
  R2s_adj[is_quadratic] <- R2s[is_quadratic] - penalty_quadratic
  i_best <- which.max(R2s_adj)
  rlang::list2(
    "best_model" = models[[i_best]]$model,        # Best model by R2
    "best_model_name" = names(models)[i_best],
    "R2s" = R2s,
    "R2s_adj" = R2s_adj,
    "n_conc" = length(unique(conc[!is.na(value)])),      # Number of unique concentrations
    "inv_model" = models[[i_best]]$inv_mod,
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

#' Compute the LLO(Q/D)
#'
#' @param v A numeric vector of the peak areas of the calibration samples
#' @param min_conc The lower limit concentration of calibration curve
#' @param calcurve_model The calibration curve model
#' @param conc A numeric vector of the concentrations of the calibration samples. It should be
#'   the same length as the `v`.
#' @param times Multiplication factor to the mean of the peak area of the `min_conc`
#'   concentration. Common values are 3 and 10 for LLOD and LLOQ, respectively. 
#' @returns A numeric value of the LLO(Q/D)
#' @export
compute_llox <- function(v, conc, min_conc, calcurve_model, times) {
  stopifnot(length(conc) == length(v))       # Identical concentrations
  # Standard deviation of the lowest non-zero concentration
  v <- v[conc == min_conc]
  s <- stats::sd(v, na.rm = TRUE)
  
  # Slope of calibration curve
  e <- environment(calcurve_model$best_model)
  slope <- if (grepl("linear", calcurve_model$best_model_name)) {
    get("b1", envir = e)
  } else {
    a <- get("a", envir = e)
    b <- get("b", envir = e)
    2 * a * min_conc + b
  }
  # Calculate the LLOx
  (s * times) / slope
}

#' Compute the concentration of features
#'
#' @param x_se A [`SumExp`] object of the samples
#' @param cc_se A [`SumExp`] object of the calibration samples
#' @param calcurve_models A list of calibration curve models
#' @param mat_id The name of a matrix in `x_se`
#'
#' @returns A matrix of the concentration of features
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

#' Replace the values below the LLOQ and LLOD
#' 
#' @param conc A matrix of concentrations
#' @param limits A data frame of the LLOQ and LLOD. The names of the columns in `limits` should 
#'   have `lloq` and `llod`.
#' 
#' @returns A matrix with the values below the LLOQ and LLOD replaced
#' @export
replace_below_lloq_llod <- function(conc, limits) {
  stopifnot(nrow(conc) == nrow(limits))
  lab <- labelled::get_label_attribute(conc)
  out <- conc
  # Replace the values below LLOQ with half of the LLOQ
  out <- ifelse(conc < limits$lloq, limits$lloq / 2, out)
  # Replace the values below LLOD with 1/4 of the LLOQ
  out <- ifelse(conc < limits$llod, limits$lloq / 4, out)
  labelled::set_label_attribute(out, lab)
}


