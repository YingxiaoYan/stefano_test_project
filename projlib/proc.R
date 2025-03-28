box::use(util = ./msdial_utils)

#' Add intermediate data at each step of quality control
#'
#' @param name A character string for the name of the intermediate data
#' @param data The data to be stored with the `name`
#' @param file A file name to store the intermediate data
#' @md
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

#' Stop without showing error messages
#'
#' @export
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# Normalization using internal standards -------------------------------------------------

## Volumetric normalization -------------------

#' Normalize the data by the volumetric internal standard
#'
#' @param se A [`SumExp::SumExp`] object
#' @param is_vIS A logical vector indicating the volumetric internal standard. Only one 
#'   volumetric internal standard is allowed. The length of the vector should be the same as
#'   the number of rows of `se`.
#' @param mat_id The name of a matrix in `se` to be normalized
#' @returns A [`SumExp::SumExp`] object with the volumetric normalization. The normalized
#'   matrix is added to the `se` with the name `vol_norm`.
#' @md
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

## Data cleaning ------------------

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
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @returns A numeric vector of the number of outlying internal standard features per sample
#' @md
#' @export
count_outliers_per_sample <- function(se, mat_id, times = 3) {
  x <- se[[mat_id]]
  x <- log1p(x)              # Log-transform the data
  outlying <- apply(x, 1, identify_outliers, times = times)
  return(rowSums(outlying))     # Transposed by `apply` above
}

## Normalization methods ------------------

#' Get the values of the closest internal standard features
#'
#' @param se A [`SumExp::SumExp`] object
#' @param istd_se A [`SumExp::SumExp`] object of internal standard features
#' @param mat_id The name of a matrix in `se` 
#' @param rt The name of the retention time column 
#' @returns A numeric matrix of the values of the closest internal standard features
#' @md
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

#' Get the LOESS fit model
#'
#' @param istd_se A [`SumExp::SumExp`] object of internal standard features
#' @param excl_cat A character vector of the categories to exclude
#' @param overall_rt_range A numeric vector of the overall retention time range, to which the
#'   model is expanded
#' @param span A numeric value for the span of the LOESS fit
#' @param mat_id The name of a matrix in `istd_se`
#' @param rt The name of the retention time column
#'
#' @returns A list of the LOESS fit models
#' @md
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, mat_id, rt = "rt") {
  # Log-transform the data
  istd_log <- log(istd_se[[mat_id]])
  rt_istd <- SumExp::row_df(istd_se)[[rt]]
  
  # Normalize the data using the internal standards
  # Mean of each internal standard feature for overall measurement samples
  # , excluding the calibration curve and blank samples
  to_excl <- util$contr_cat(istd_se) %in% excl_cat
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

#' Get the matrix ID of the blank-subtracted matrix
#'
#' @param mat_id The name of a matrix 
#'
#' @returns The name of the blank-subtracted matrix
#' @export
get_mat_id_of_blank_subtracted <- function(mat_id) {
  paste0(mat_id, "_blk")
}

#' Subtract the average values of the blank samples from the samples
#'
#' @param x_se A [`SumExp::SumExp`] object of the samples
#' @param no_change A condition to select the samples that should not be changed.
#' @param mat_ids The names of matrices in `x_se`, from which the blank values are subtracted
#' @param out_mat_ids The names of the output matrices in the returned object. 
#'   These should be the same size as `mat_ids` in the same order.
#'
#' @returns A [`SumExp::SumExp`] object with the blank values subtracted. 
#'   The columns of the blanks are removed from the `x_se`.
#' @md
#' @export
add_blank_substracted_sumexp <- function(x_se, 
                                         no_change,
                                         mat_ids, 
                                         out_mat_ids) {
  stopifnot(length(mat_ids) == length(out_mat_ids))
  g <- ifelse(util$contr_cat(x_se) == "Blank", "Blank", "Other")
  se <- SumExp::split_columns(x_se, g)
  blank_se <- se[["Blank"]]
  x_se <- se[["Other"]]
  no_change <- no_change[g == "Other"]    # Blank has been split
  for(ii in seq(mat_ids)) {      # Paired `mat_ids` and `out_mat_ids`
    mat_id <- mat_ids[ii]
    x_mat <- x_se[[mat_id]]
    x_lab <- labelled::get_label_attribute(x_mat)
    # Mean per feature
    blank_mean <- rowMeans(blank_se[[mat_id]])
    # Blank means are saved in the row_df of x_se with this name
    r_nm <- paste0(mat_id, "_blank_mean")
    SumExp::row_df(x_se)[[r_nm]] <- blank_mean |> 
      labelled::set_label_attribute(paste("Blank mean of", x_lab))
    
    # <<---- Subtract by blank average ---->> #
    mat_subt <- x_mat - blank_mean
    
    mat_subt[mat_subt < 0] <- 0
    mat_subt[, no_change] <- x_mat[, no_change]
    x_se[[ out_mat_ids[ii] ]] <- mat_subt |> 
      labelled::set_label_attribute(paste(x_lab, "(blank adjusted)"))
  }
  x_se
}

# Calibration ----------------------------------------------------------------------------

## Identify minimum/maximum concentration points ----------

#' Split a matrix-like object by the spiked concentration points and sort by the values
#'
#' @param cc_se A [`SumExp::SumExp`] object of the calibration curve samples
#' @param mat_id A matrix ID in the `cc_se`
#'
#' @returns A list of matrices of `mat_id` split column-wise. The list is sorted by the values.
#' @md
mat_split_columns_and_sort_by_spiked_conc <- function(cc_se, mat_id) {
  # Split the columns by the values
  conc <- util$spiked_conc_pts(cc_se)
  sorted <- stats::setNames(nm = sort(unique(conc)))
  lapply(sorted, \(.x) cc_se[[mat_id]][, conc == .x, drop = FALSE])
}

#' Apply a function to the values in `mat_id` of spiked concentration points
#' 
#' @inheritParams mat_split_columns_and_sort_by_spiked_conc
#' @inheritParams base::apply
#' @param FUN A function to apply to the values in `mat_id` of spiked concentration points. 
#'   Ideally, the function should return a single value.
#' @returns A matrix (or vector when only one point) of the results of applying `FUN` to the
#'   values in `mat_id` per spiked concentration point
#' @md
apply_per_spiked_conc <- function(cc_se, mat_id, MARGIN, FUN, ...) {
  cc_lst <- mat_split_columns_and_sort_by_spiked_conc(cc_se, mat_id)
  sapply(cc_lst, apply, MARGIN, FUN, ...)
}


#' Identify LOD/LLOQ signal
#' 
#' @param mat_m A matrix of the mean signals per spiked concentration point
#' @param mat_sd A matrix of the standard deviation signals per spiked concentration point
#' @param non_zero A vector of the lowest concentration points with non-zero mean value.
#'   The length of the vector should be the same as the number of rows of the matrices in
#'   `mat_m` and `mat_sd`
#' @param times Multiplication factor to the standard deviation of the signal to get the LOD or
#'   LLOQ. Common values are 3 and 10 for LOD and LLOQ, respectively. 
#' @returns A numeric vector of the LOD or LLOQ signals
#' @md
identify_lox_signal <- function(mat_m, mat_sd, non_zero, times) {
  stopifnot(
    all(dim(mat_m) == dim(mat_sd)),
    nrow(mat_m) == length(non_zero)
  )
  conc <- as.numeric(colnames(mat_m))
  stopifnot("The columns of `mat_m` are supposed to be sorted" = all(conc == sort(conc)))
  sapply(1:nrow(mat_m), \(ii) {
    incl <- conc >= non_zero[ii]
    m <- mat_m[ii, incl]
    s <- mat_sd[ii, incl]
    is_all_0 <- m == 0      # If all values of one pt are zero, rsd is NaN
    m <- m[!is_all_0]
    s <- s[!is_all_0]
    rsd <- s / m 
    m[1] + times * mean(rsd) * m[1]    # Mean of non-zero + ...
  })
}

#' Identify the minimum concentration point for calibration curve
#' 
#' @inheritParams identify_lox_signal
#' @param lod_signal A numeric vector of the LOD signals
#' @returns A numeric vector of the minimum concentrations
#' @md
min_conc_for_curve <- function(mat_m, lod_signal) {
  stopifnot(nrow(mat_m) == length(lod_signal))
  # The minimum concentration having higher signal than the LOD
  out <- apply(mat_m > lod_signal, 1, util$get_min_satisfy)
  nm <- names(out)
  out <- as.numeric(out)         # Lost names by this
  names(out) <- nm
  out <- labelled::set_label_attribute(out, "Minimum Concentration")
  out
}

#' Identify the maximum concentration point of the calibration samples
#'
#' @inheritParams identify_lox_signal
#' @param mat_q A matrix of the peak areas of the samples of interest
#' @param times A numeric value to multiply the maximum peak area of the samples for
#'   measurement to get a margin for the maximum concentration
#'
#' @returns A numeric vector of the upper limit concentration for the calibration curve.
#'   If all values of `mat_qe` are zero for a row, the maximum concentration is zero. 
#'   The names of the returned vector are the row names. 
#' @md
max_conc_for_curve <- function(mat_m, mat_q, times) {
  stopifnot(
    nrow(mat_m) == nrow(mat_q),
    identical(rownames(mat_m), rownames(mat_q))
  )
  # Find maximum peak area of the samples of non-calibration samples
  max_in_q <- apply(mat_q, 1, max, na.rm = TRUE)
  # Find the maximum concentration of the calibration samples that are not too far (`times`x)
  out <- apply(mat_m <= times * max_in_q, 1, util$get_max_satisfy)
  out[max_in_q == 0] <- 0
  out[is.na(out)] <- -9
  out <- labelled::set_label_attribute(out, "Maximum Concentration")
  stopifnot(identical(names(out), rownames(mat_q)))
  out
}

#' Make sure to have enough calibration curve samples
#'
#' @param max_conc A vector of the maximum concentration
#' @param min_conc A vector of the minimum concentration
#' @param conc A numeric vector of spiked concentrations
#' @param min_n Required minimum number of concentrations for the calibration curve. If the 
#'   number of concentrations is less than this, `NA` is returned.
#' @param enough_n Enough number of concentrations for the calibration curve
#'
#' @returns A numeric vector of the maximum concentrations that provide enough number of spiked
#'   concentrations. The names are the same as the input vectors `max_conc`. If the number of
#'   concentrations is less than `min_n`, the maximum concentration is set to `NA`
#' @md
make_sure_to_have_enough_calcurve <- function(max_conc, min_conc, conc_pts, 
                                              min_n = 3, enough_n = 5) {
  stopifnot(length(max_conc) == length(min_conc))
  uniq_conc <- sort(unique(conc_pts))
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

#' Limits in the calibration curve
#'
#' @name calibration_limit_pts
#' 
#' @param x_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @param mat_id The name of a matrix in `x_se`
#' @md
NULL
#' @rdname calibration_limit_pts
#' @aliases find_calibration_limit_pts
#' @returns **`find_calibration_limit_pts`** & **`extract_calibration_limit_pts`** :
#' 
#'   A data frame with the limits. 
#'   The row names are the names of the calibration curve samples. 
#'   The columns are `non_zero_conc`, `min_c_conc`, `max_c_conc`, `lod_signal`, and
#'   `lloq_signal`, which are the non-zero spiked concentration, the lower and upper limits of
#'   the calibration curve, the LOD signal, and the LLOQ signal, respectively.
#'   Some values in the columns are set to have the following meanings:
#'   * `min_c_conc` = NA : No valid concentration even at the top of the calibration curve
#'   * `max_c_conc` = 0 : All concentrations are zero
#'   * `max_c_conc` = -9 : No valid maximum concentration
#'   * `max_c_conc` = NA : Not enough calibration curve samples to make a calibration curve
#' @md
#' @export
find_calibration_limit_pts <- function(x_se, mat_id) {
  se <- util$split_into_calcurve_and_other(x_se, out_names = c("cc", "quant"))
  
  # Means/standard deviation per spiked concentration point
  mat_m <- apply_per_spiked_conc(se$cc, mat_id, 1, mean, na.rm = TRUE)
  mat_sd <- apply_per_spiked_conc(se$cc, mat_id, 1, stats::sd, na.rm = TRUE)
  # The minimum concentration with non-zero mean
  non_zero <- apply(mat_m > 0, 1, util$get_min_satisfy)
  # Limit of detection
  lod_signal <- identify_lox_signal(mat_m, mat_sd, non_zero, time = 3)
  # Lower limit of quantification
  lloq_signal <- identify_lox_signal(mat_m, mat_sd, non_zero, time = 10)
  # Calibration curve concentration lower limit. The lowest concentration point
  min_c_conc <- min_conc_for_curve(mat_m, lod_signal)
  # Calibration curve concentration upper limit
  mat_q <- se$quant[, util$contr_cat(se$quant) == ""][[mat_id]]   # Excluding "QC" category
  conc_pts <- as.numeric(colnames(mat_m))
  max_c_conc <- max_conc_for_curve(mat_m, mat_q, times = 10) |>
    make_sure_to_have_enough_calcurve(min_c_conc, conc_pts, min_n = 3, enough_n = 5)
  
  stopifnot(identical(names(min_c_conc), names(max_c_conc)))
  stopifnot(identical(names(non_zero), names(max_c_conc)))
  out <- data.frame(non_zero_conc = non_zero, min_c_conc, max_c_conc, 
                    lod_signal, lloq_signal)
  stopifnot(identical(rownames(out), rownames(x_se)))
  out
}
#' @rdname calibration_limit_pts
#' @aliases add_calibration_curve_limits
#' @returns **`add_calibration_curve_limits`**:
#'   A [`SumExp::SumExp`] object with the calibration curve limits
#' @md
#' @export
add_calibration_curve_limits <- function(x_se, mat_id) {
  l_df <- find_calibration_limit_pts(x_se, mat_id)
  SumExp::row_df(x_se) <- cbind(SumExp::row_df(x_se), l_df)
  x_se
}
#' @description
#'   **`extract_calibration_limit_pts`**: 
#'   Extract the calibration curve limits from the output of [`add_calibration_curve_limits()`]
#' @rdname calibration_limit_pts
#' @aliases extract_calibration_limit_pts
#' @md
#' @export
extract_calibration_limit_pts <- function(x_se) {
  SumExp::row_df(x_se)[, c("non_zero_conc", "min_c_conc", "max_c_conc", 
                           "lod_signal", "lloq_signal")]
}
#' @description
#'   **`get_calibration_nonzero_pts`**: 
#'   Extract the lowest non-zero concentration from the output of 
#'   [`add_calibration_curve_limits()`]
#' @rdname calibration_limit_pts
#' @aliases get_calibration_nonzero_pts
#' @md
#' @export
get_calibration_nonzero_pts <- function(x_se) {
  SumExp::row_df(x_se)[["non_zero_conc"]]
}
#' @description
#'   **`get_calibration_min_pts`**: 
#'   Extract the minimum limits from the output of [`add_calibration_curve_limits()`]
#' @rdname calibration_limit_pts
#' @aliases get_calibration_min_pts
#' @md
#' @export
get_calibration_min_pts <- function(x_se) {
  SumExp::row_df(x_se)[["min_c_conc"]]
}
#' @description
#'   **`get_calibration_max_pts`**: 
#'   Extract the maximum limits from the output of [`add_calibration_curve_limits()`]
#' @rdname calibration_limit_pts
#' @aliases get_calibration_max_pts
#' @md
#' @export
get_calibration_max_pts <- function(x_se) {
  SumExp::row_df(x_se)[["max_c_conc"]]
}

#' Check if the calibration curve has a proper calibration range
#'
#' @param x_se A [`SumExp::SumExp`] object of the calibration curve samples. 
#'   The object should have the calibration curve limits added by
#'   [`add_calibration_curve_limits()`]
#'
#' @returns A logical vector
#' @md
#' @export
has_proper_calibration_range <- function(x_se) {
  min_c_conc <- get_calibration_min_pts(x_se)
  max_c_conc <- get_calibration_max_pts(x_se)
  # No valid concentration point even at the top of the calibration curve
  no_valid_conc <- is.na(min_c_conc)
  # All concentrations are zero
  all_zero <- max_c_conc == 0
  # No valid maximum concentration
  no_max_conc <- max_c_conc == -9
  # Not enough calibration curve samples to make a calibration curve
  not_enough <- is.na(max_c_conc)
  !(no_valid_conc | all_zero | no_max_conc | not_enough)
}


#' Replace the values outside the concentration range with NA
#'
#' @param x [`SumExp::SumExp`] object or a matrix of the calibration curve samples
#' @param ... Additional arguments. Not used. 
#' @param mat_id The name of a matrix in `x` when `x` is a [`SumExp::SumExp`] object
#' @param conc A vector of concentrations of the calibration curve samples (`matrix`) 
#' @param min_conc,max_conc A vector of minimum and maximum valid concentrations (`matrix`)
#'
#' @returns An object of the same class as `x` with the values outside the concentration range
#'   replaced with NA
#' @md
#' @export
replace_outside_concentration_range_with_na <- function(x, ...) {
  UseMethod("replace_outside_concentration_range_with_na")
}
#' @rdname replace_outside_concentration_range_with_na
#' @method replace_outside_concentration_range_with_na matrix
#' @export
replace_outside_concentration_range_with_na.matrix <- 
  function(x, conc, min_conc, max_conc) {
    stopifnot(
      length(conc) == ncol(x),
      length(min_conc) == nrow(x),
      length(max_conc) == nrow(x)
    )
    # Concentration values in a matrix, rows are features and columns are concentrations
    conc_mat <- matrix(rep(conc, each = nrow(x)), nrow = nrow(x))
    to_na <- conc_mat < min_conc | conc_mat > max_conc
    x[to_na] <- NA_real_
    x
  }
#' @rdname replace_outside_concentration_range_with_na
#' @method replace_outside_concentration_range_with_na SumExp
#' @export
replace_outside_concentration_range_with_na.SumExp <- function(x, mat_id) {
  x[[mat_id]] <- replace_outside_concentration_range_with_na.matrix(
    x = x[[mat_id]], 
    conc = util$spiked_conc_pts(x),
    min_conc = get_calibration_min_pts(x),
    max_conc = get_calibration_max_pts(x)
  )
  x
}

#' Get the signals of the calibration curve samples at the minimum valid concentration
#' 
#' @param x_se A [`SumExp::SumExp`] object of the calibration curve samples.
#' @param mat_id The name of a matrix in `x_se`
#' 
#' @returns A list of numeric vectors of the signals of the calibration curve samples at the
#'   minimum valid concentration
#' @md
#' @export
get_signals_of_calibration_nonzero_pts <- function(x_se, mat_id) {
  cc_mat <- x_se[[mat_id]]
  nonzero <- get_calibration_nonzero_pts(x_se)
  cc_conc <- util$spiked_conc_pts(x_se)
  stats::setNames(1:nrow(x_se), nm = rownames(x_se)) |>  # Through the rows
    lapply(function(i) cc_mat[i, cc_conc == nonzero[i]])
}


## Calibration curve fitting ----------

#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param signal A vector of signal values
#' @param penalty_quadratic The penalty for the quadratic models
#'
#' @returns A list with the best model, the name of it, the R2 values of all models, and the
#'   number of unique concentrations. The best model is chosen as the model with the highest R2
#'   after applying the penalty to quadratic models. 
#' @examples
#' conc <- rep(c(0.1, 0.2, 0.5, 1, 2), 3)
#' signal <- conc * 5 + rnorm(length(conc))
#' fit_and_test_calcurve_model(conc, signal)
#' @name calcurve_model
#' @md
NULL
#' @rdname calcurve_model
#' @export
fit_and_test_calcurve_model <- function(conc, signal, penalty_quadratic = 0) {
  if (all(is.na(signal) | signal == 0)) {
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
  lmodels <- lapply(weights_alt, \(w) linear_calcurve_model(conc, signal, weights = w))
  names(lmodels) <- paste("linear", names(lmodels), sep = "-")
  qmodels <- lapply(weights_alt, \(w) quadratic_calcurve_model(conc, signal, weights = w))
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
    "n_conc" = length(unique(conc[!is.na(signal)])),      # Number of unique concentrations
    "inv_model" = models[[i_best]]$inv_mod,
  )
}
#' @param weights weights for linear model fit
#' @rdname calcurve_model
quadratic_calcurve_model <- function(conc, signal, weights) {
  lmfit <- stats::lm(signal ~ conc + I(conc^2), weights = weights)
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
linear_calcurve_model <- function(conc, signal, weights) {
  lmfit <- stats::lm(signal ~ conc, weights = weights)
  beta <- stats::coef(lmfit)
  b1 <- beta[["conc"]]
  b0 <- beta[["(Intercept)"]]
  model <- function(x) {
    (x - b0) / b1
  }
  list(model = model, inv_mod = lmfit)
}


## LOD/LLOQ ----------


#' Compute the LLO(Q/D)
#'
#' @param v A numeric vector of the peak areas of the calibration samples
#' @param min_conc The lower limit concentration of calibration curve
#' @param calcurve_model The calibration curve model
#' @param conc A numeric vector of the concentrations of the calibration samples. It should be
#'   the same length as the `v`.
#' @param times Multiplication factor to the mean of the peak area of the `min_conc`
#'   concentration. Common values are 3 and 10 for LOD and LLOQ, respectively. 
#' @returns A numeric value of the LLO(Q/D)
#' @md
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
#' @param x_se A [`SumExp::SumExp`] object of the samples
#' @param mat_id The name of a matrix in `x_se`
#'
#' @returns A matrix of the concentration of features
#' @md
#' @export
compute_concentration <- function(x_se, mat_id) {
  mat <- x_se[[mat_id]]
  models <- SumExp::row_df(x_se)$calcurve_model
  # Calculate the concentration of each feature
  conc <- sapply(rownames(mat), \(i_feature) {
    v <- mat[i_feature, ]
    # Concentration by the best model
    models[[i_feature]]$best_model(v)
  }) |> 
    t()          # Features to rows
  conc <- labelled::set_label_attribute(conc, "Concentration")
  conc
}

#' Replace the values below the LLOQ and LOD
#' 
#' @param conc A matrix of concentrations
#' @param limits A data frame of the LLOQ and LOD. The names of the columns in `limits` should 
#'   have `lloq` and `lod`.
#' 
#' @returns A matrix with the values below the LLOQ and LOD replaced
#' @md
#' @export
replace_below_lloq_llod <- function(conc, limits) {
  stopifnot(nrow(conc) == nrow(limits))
  lab <- labelled::get_label_attribute(conc)
  out <- conc
  # Replace the values below LLOQ with half of the LLOQ
  out <- ifelse(conc < limits$lloq, limits$lloq / 2, out)
  # Replace the values below LOD with 1/4 of the LLOQ
  out <- ifelse(conc < limits$lod, limits$lloq / 4, out)
  labelled::set_label_attribute(out, lab)
}


