# ------------------------------------------------------------------------------------------- #
# Functions for processing of the data
# ------------------------------------------------------------------------------------------- #
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
#' @param verbose A logical value indicating whether to show messages
#' @returns A numeric matrix of the values of the closest internal standard features
#' @md
#' @export
get_value_of_closest_istd <- function(se, istd_se, mat_id, verbose = TRUE) {
  out <- get_value_of_closest_istd_in_class(se, istd_se, mat_id)
  
  # When any compound target classes are specified
  istd_std_type <- util$std_type(istd_se)
  is_sub_istd <- grepl("^IS\\\\", istd_std_type)
  if (any(is_sub_istd)) {
    # Identify the compound target classes
    classes <- unique(istd_std_type[is_sub_istd])
    if (verbose) cat("Compound target classes: ", paste(classes, collapse = ", "), "\n")
    # Get the closest internal standard feature for each subgroup
    for (g in classes) {
      g_istd_se <- istd_se[istd_std_type == g, ]
      matched_quant_id <- sub("^IS\\\\", "Quant\\\\", g)
      # To find which target features are in which compound target class
      is_sub_quant <- util$std_type(se) == matched_quant_id
      g_se <- se[is_sub_quant, , drop = FALSE]
      if (nrow(g_se) == 0) {
        warning(paste0("No quantification features for the compound class: ", g))
        next
      }
      g_out <- get_value_of_closest_istd_in_class(g_se, g_istd_se, mat_id)
      # Replace the values of the closest internal standard features in the original matrix
      # with the values of the closest internal standard features in the compound class
      out[is_sub_quant, ] <- g_out
    }
  }
  return(out)
}
#' @rdname get_value_of_closest_istd
#' [get_value_of_closest_istd_in_class()] is a helper function to get the values of the
#' closest internal standard features within each subgroup.
#' @md
#' @export
get_value_of_closest_istd_in_class <- function(se, istd_se, mat_id) {
  rt_x <- util$retention_time(se)
  rt_istd <- util$retention_time(istd_se)
  # Find the closest internal standard feature
  i_closest <- sapply(rt_x, \(.x) which.min(abs(rt_istd - .x)))
  # dim(out) == dim(se) Not dim(istd_se)
  out <- istd_se[[mat_id]][i_closest, , drop = FALSE]
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
#'
#' @returns A list of the LOESS fit models
#' @md
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, mat_id) {
  # Log-transform the data
  istd_log <- log(istd_se[[mat_id]])
  rt_istd <- util$retention_time(istd_se)
  
  # Normalize the data using the internal standards
  # Mean of each internal standard feature for overall measurement samples
  # , excluding the calibration curve and blank samples
  mat <- log(util$exclude_ctrl_smpl_cat(istd_se, excl_cat)[[mat_id]])
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
mat_id_of_blank_subtracted <- function(mat_id) {
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
  g <- ifelse(util$ctrl_smpl_cat(x_se) == "Blank", "Blank", "Other")
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

## Calibration working range ----------

### Utils ----------

#### For matrices of spiked concentration points ----------

#' Get the values satisfying the condition
#' 
#' @param cond A logical vector to select the values.
#' @param values A numeric or character vector of values. 
#'   When it is a character vector, the values are converted to numeric.
#' 
#' @returns The `values` that satisfies the condition `cond`. 
#'   If no value satisfies the condition, return `NA_real_`.
#' @md
satisfying_values <- function(cond, values = names(cond)) {
  stopifnot(length(cond) == length(values))
  values <- as.numeric(values)
  if (all(!cond)) return(NA_real_)
  values[which(cond)]
}

#' Split a matrix column-wise by the spiked concentration points and sort by the values
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

#' Get means, standard deviation and non-zero concentration
#'
#' @description
#' The output is a list with the following elements:
#' - `mat_m` and `mat_sd` are matrices with the mean and standard deviation of the signal 
#'   values for each chemical and concentration point.
#' - `non_zero` is a vector of the concentration points with non-zero signal for each chemical.
#' - `conc` is a vector of the concentration points.
#' 
#' @param cc_se A [`SumExp::SumExp`] object of the calibration curve
#' @param mat_id A matrix ID
#' @returns A list with `mat_m`, `mat_sd`, `non_zero` and `conc`
#' @md
list_mean_sd_and_nonzero <- function(cc_se, mat_id) {
  # Mean/standard deviation per spiked concentration point
  # Columns: spiked concentration points (sorted), Rows: chemicals
  mat_m  <- apply_per_spiked_conc(cc_se, mat_id, 1, mean, na.rm = TRUE)
  mat_sd <- apply_per_spiked_conc(cc_se, mat_id, 1, stats::sd, na.rm = TRUE)
  # Concentration values
  conc <- as.numeric(colnames(mat_m))    # In fact, no need
  # The minimum concentration with non-zero mean
  non_zero <- apply(mat_m > 0, 1, \(.x) min(satisfying_values(.x, conc)))
  
  list(mat_m = mat_m, mat_sd = mat_sd, non_zero = non_zero)
}

#' Get the values of the non-zero concentration points
#'
#' @param mat A matrix of values, in which the rows are chemicals and the columns are
#'   concentration points. The column names should be the concentration values at each point.
#' @param non_zero A vector of the concentration points with non-zero signal for each chemical
#'
#' @returns A vector of the values of the non-zero concentration points
values_of_non_zero <- function(mat, non_zero) {
  stopifnot(nrow(mat) == length(non_zero))
  # Concentration values
  conc <- as.numeric(colnames(mat))
  stopifnot(all(non_zero %in% conc), anyDuplicated(conc) == 0)
  # Extract the mean of the lowest non-zero concentration point
  sapply(1:nrow(mat), \(ii) {
    mat[ii, conc == non_zero[ii]]
  })
}

### LOD/LLOQ ----------

#' Compute the LOD/LLOQ
#' 
#' @name compute_llox
#' 
#' @param mat_m A matrix of the mean signals per spiked concentration point. 
#'   The columns are the points.
#' @param mat_sd A matrix of the standard deviation signals per spiked concentration point.
#'   The columns are the points.
#' @param non_zero A vector of the lowest concentration points with non-zero mean value.
#'   The length of the vector should be the same as the number of rows of the matrices in
#'   `mat_m` and `mat_sd`
#' @param times Multiplication factor to the standard deviation of the signal to get the LOD or
#'   LLOQ. Common values are 3 and 10 for LOD and LLOQ, respectively. 
#' 
#' @returns A numeric vector of the LOD/LLOQ signal values for each chemical
NULL

#### LOD/LLOQ from signal perspective ----------

#' Compute the LOD/LLOQ signal using mean + SD * times
#'
#' @inheritParams compute_llox
#' @inherit compute_llox returns
#' @export
compute_llox_signal_using_mean_plus_sd_times <- function(mat_m, mat_sd, non_zero, times) {
  stopifnot(all(dim(mat_m) == dim(mat_sd)))
  # Extract the mean and standard deviation of the lowest non-zero concentration point
  m_nz <- values_of_non_zero(mat_m, non_zero)
  sd_nz <- values_of_non_zero(mat_sd, non_zero)
  # Compute LOD/LLOQ using mean + SD * times
  llox_signal <- m_nz + times * sd_nz
  # Set the chemical IDs as names 
  names(llox_signal) <- rownames(mat_m)
  llox_signal
}

#' Compute the LOD/LLOQ signal using mean * times
#'
#' @inheritParams compute_llox
#' @param mat_sd Not used. It is included to keep the same function signature as other
#'   `compute_llox*` functions.
#' @inherit compute_llox returns
#' @export
compute_llox_signal_using_mean_times <- function(mat_m, mat_sd, non_zero, times) {
  # Extract the mean of the lowest non-zero concentration point
  m_nz <- values_of_non_zero(mat_m, non_zero)
  # Compute LOD/LLOQ using mean * times
  llox_signal <- m_nz * times
  # Set the chemical IDs as names 
  names(llox_signal) <- rownames(mat_m)
  llox_signal
}

#' Compute the LOD/LLOQ signal using mean plus average RSD
#' 
#' This function computes a value for the limit of detection (LOD) or lower limit of
#' quantification (LLOQ) from signal intensity perspective. It is based on the mean signal of
#' the lowest concentration point `m` and the average of relative standard deviation values
#' (a.k.a. coefficient of variation) `rsd` of the signal intensity at each concentration point. 
#' The formula is:  `m + mean(rsd) * m * times`
#' , where `times` is a multiplication factor
#' 
#' @inheritParams compute_llox
#' @inherit compute_llox returns
#' @md
#' @export
compute_llox_signal_using_nonzero_mean_and_avg_rsd <- function(mat_m, mat_sd, non_zero, times) {
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

#### LOD/LLOQ at spiked concentration points ----------

#' Get the minimum concentration points that satisfy the condition
#' 
#' @param cond A logical matrix, where each row is a chemical and each column is a
#'   concentration point.
#' @returns A numeric vector of the same length as the number of rows in `cond`.
#' @md
#' @export
identify_min_pt_satisfying <- function(cond) {
  llox_pt <- apply(cond, 1, \(.x) min(satisfying_values(.x)))
  # Set the chemical IDs as names
  names(llox_pt) <- rownames(cond)
  llox_pt
}

#### LOD/LLOQ concentration values ----------

#' Compute the LLO(Q/D) using the slope and standard deviation
#' 
#' `s * factor / slope`
#' , where `s` the standard deviation of the lower-limit concentration point, 
#' `slope` is the slope of the calibration curve at the point, and `factor` is a multiplication
#' factor.
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
compute_llox_using_slope_and_sd <- function(v, conc, min_conc, calcurve_model, times) {
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

### With derived min/max concentration ----------

#' Identify the minimum concentration point for calibration curve
#' 
#' @param mat_m A matrix of the mean signals per spiked concentration point. The columns are
#'   the spiked concentration points, which should be sorted in ascending order.
#' @param lod_signal A numeric vector of the LOD signals
#' @returns A numeric vector of the minimum concentrations
#' @md
min_conc_for_curve_using_lox_signal <- function(mat_m, lod_signal) {
  stopifnot(nrow(mat_m) == length(lod_signal))
  # The minimum concentration having higher signal than the LOD
  out <- apply(mat_m > lod_signal, 1, \(.x) min(satisfying_values(.x)))
  nm <- names(out)
  out <- as.numeric(out)         # Lost names by this
  names(out) <- nm
  out <- labelled::set_label_attribute(out, "Minimum Concentration")
  out
}

#' Identify the maximum concentration point of the calibration samples
#'
#' @param mat_m A matrix of the mean signals per spiked concentration point. The columns are
#'   the spiked concentration points, which should be sorted in ascending order.
#' @param mat_q A matrix of the peak areas of the samples of interest
#' @param times A numeric value to multiply the maximum peak area of the samples for
#'   measurement to get a margin for the maximum concentration
#'
#' @returns A numeric vector of the upper limit concentration points for the calibration curve.
#'   The names of the returned vector are the row names. 
#'   Some returned values mean:
#'   * `0`  : All values of `mat_q` are zero for the row
#'   * `-9` : No valid maximum concentration
#' @md
max_conc_for_curve <- function(mat_m, mat_q, times) {
  stopifnot(
    nrow(mat_m) == nrow(mat_q),
    identical(rownames(mat_m), rownames(mat_q))
  )
  # Find maximum peak area of the samples of non-calibration samples
  max_in_q <- apply(mat_q, 1, max, na.rm = TRUE)
  # Find the maximum concentration of the calibration samples that are not too far (`times`x)
  out <- apply(mat_m <= times * max_in_q, 1, \(.x) max(satisfying_values(.x)))
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
#' @param conc_pts A numeric vector of spiked concentrations
#' @param min_n Required minimum number of concentrations for the calibration curve. If the 
#'   number of concentrations is less than this, `NA` is returned.
#' @param enough_n Enough number of concentrations for the calibration curve
#'
#' @returns A numeric vector of the maximum concentrations that provide enough number of spiked
#'   concentrations. The names are the same as the input vector `max_conc`. If the number of
#'   concentrations points is less than `min_n`, the returned value is set to `NA_real_`
#' @md
make_sure_to_have_enough_calcurve <- function(max_conc, min_conc, conc_pts, 
                                              min_n = 3, enough_n = 5) {
  stopifnot(length(max_conc) == length(min_conc))
  uniq_conc <- sort(unique(conc_pts))
  n_uniq_conc <- length(uniq_conc)
  # Indices of the minimum concentration points
  i_min <- match(min_conc, uniq_conc)
  # Find the indices of the maximums that provide enough number of spiked concentration points
  i_max <- match(max_conc, c(0, uniq_conc)) - 1   # If `max_conc` == 0, take the largest range
  # Counting from minimum
  i_enough_from_min <- i_min + enough_n - 1
  i_max <- ifelse(i_enough_from_min <= i_max, i_max, i_enough_from_min)
  # Limit when `i_enough_from_min` > number of concentration points
  i_max <- ifelse(i_max > n_uniq_conc, n_uniq_conc, i_max)
  # If the number of valid concentration points is less than `min_n`, return NA
  i_max <- ifelse(i_max >= i_min + min_n - 1, i_max, NA_real_)
  out <- uniq_conc[i_max]
  names(out) <- names(max_conc)
  out <- labelled::copy_labels(max_conc, out)
  stopifnot(identical(names(out), names(max_conc)))
  out
}

#### Find calibration working range ----------


#' Find the calibration curve limits and the LOD/LLOQ
#' 
#' @param x_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @param mat_id The name of a matrix in `x_se`
#' @param compute_llox_signal_fun A function to compute the LOD and LLOQ signals
#' @returns
#'   A data frame with all the limits. The row names are the chemical IDs.
#'   The columns are `non_zero_conc`, `min_c_conc`, `max_c_conc`, `lod`, and `lloq`, which are
#'   the non-zero spiked concentration, the lower and upper limits of the calibration curve,
#'   the LOD and LLOQ, respectively.
#'   Some values in the columns are set to have the following meanings:
#'   * `min_c_conc` = NA : No valid concentration even at the top of the calibration curve
#'   * `max_c_conc` = NA : Not enough calibration curve samples to make a calibration curve
#'   * `max_c_conc` = 0  : All values are zero for the row
#'   * `max_c_conc` = -9 : No valid maximum concentration
#' @md
#' @seealso [max_conc_for_curve()]
#' @export
find_calibration_limit_pts_and_llox_from_llox_signal <- function(x_se, 
                                                                 mat_id, 
                                                                 compute_llox_signal_fun) {
  se <- util$split_into_calcurve_and_other(x_se, out_names = c("cc", "quant"))
  lx <- list_mean_sd_and_nonzero(se$cc, mat_id)
  mat_m <- lx$mat_m             # `mat_m` is the mean signal matrix
  mat_sd <- lx$mat_sd           # `mat_sd` is the standard deviation matrix
  non_zero <- lx$non_zero       # `non_zero` is the concentration points with non-zero signals
  # Limit of detection
  lod_signal <- compute_llox_signal_fun(mat_m, mat_sd, non_zero, times = 3)
  stopifnot(identical(rownames(mat_m), names(lod_signal)))
  lod <- apply(mat_m >= lod_signal, 1, \(.x) min(satisfying_values(.x)))
  # Lower limit of quantification
  lloq_signal <- compute_llox_signal_fun(mat_m, mat_sd, non_zero, times = 10)
  stopifnot(identical(rownames(mat_m), names(lloq_signal)))
  mat_rsd <- mat_sd / mat_m
  mat_cond <- mat_m >= lloq_signal & mat_rsd <= 0.2
  lloq <- apply(mat_cond, 1, \(.x) min(satisfying_values(.x)))
  
  # Calibration curve concentration lower limit.
  min_c_conc <- lloq
  min_c_conc <- labelled::set_label_attribute(min_c_conc, "Minimum Concentration")
  # Calibration curve concentration upper limit
  mat_q <- util$exclude_ctrl_smpl_cat(se$quant, "QC")[[mat_id]]
  conc_pts <- as.numeric(colnames(mat_m))
  max_c_conc <- max_conc_for_curve(mat_m, mat_q, times = 10) |>
    make_sure_to_have_enough_calcurve(min_c_conc, conc_pts, min_n = 3, enough_n = 5)
  
  stopifnot(
    identical(names(non_zero), names(max_c_conc)),
    identical(names(min_c_conc), names(max_c_conc))
  )
  # data.frame for SumExp::row_df, instead of tibble
  out <- data.frame(non_zero_conc = non_zero, min_c_conc, max_c_conc, lod, lloq)
  stopifnot(identical(rownames(out), rownames(x_se)))
  out
}

#### Access calibration curve limits ----------

#' Limits in the calibration curve
#'
#' @name calibration_limit_pts
#' 
#' @param x_se A [`SumExp::SumExp`] object
#' @md
NULL

#' @description
#'   **`extract_calibration_limit_pts`**: 
#'   Extract the calibration curve limits
#' @rdname calibration_limit_pts
#' @returns **`extract_calibration_limit_pts`** :
#' 
#'   A data frame with all the limits. The row names are the chemical IDs.
#'   The columns are `non_zero_conc`, `min_c_conc` and `max_c_conc`
#'   , which are the non-zero spiked concentration, the lower and upper limits of the
#'   calibration curve, respectively.
#'   Some values in the columns are set to have the following meanings:
#'   * `min_c_conc` = NA : No valid concentration even at the top of the calibration curve
#'   * `max_c_conc` = NA : Not enough calibration curve samples to make a calibration curve
#'   * `max_c_conc` = 0  : All values are zero for the row
#'   * `max_c_conc` = -9 : No valid maximum concentration
#' @md
#' @seealso [max_conc_for_curve()]
#' @export
extract_calibration_limit_pts <- function(x_se) {
  SumExp::row_df(x_se)[, c("non_zero_conc", "min_c_conc", "max_c_conc")]
}
#' @description
#'   **`calibration_nonzero_pts`**: Extract the lowest non-zero concentration
#' @rdname calibration_limit_pts
#' @md
#' @export
calibration_nonzero_pts <- function(x_se) {
  SumExp::row_df(x_se)[["non_zero_conc"]]
}
#' @description
#'   **`calibration_min_pts`**:   Extract the minimum limits
#' @rdname calibration_limit_pts
#' @md
#' @export
calibration_min_pts <- function(x_se) {
  SumExp::row_df(x_se)[["min_c_conc"]]
}
#' @description
#'   **`calibration_max_pts`**:   Extract the maximum limits
#' @rdname calibration_limit_pts
#' @md
#' @export
calibration_max_pts <- function(x_se) {
  SumExp::row_df(x_se)[["max_c_conc"]]
}

#### Post processing calibration working range setting ----------

#' Check if the calibration curve has a proper calibration range
#'
#' @param x_se A [`SumExp::SumExp`] object of the calibration curve samples. 
#'   The object should have the calibration curve limits added by
#'   [add_calibration_curve_limits()]
#'   The minimum and maximum points are required.
#'
#' @returns A logical vector
#' @seealso [extract_calibration_limit_pts()], [max_conc_for_curve()] for what the limits mean
#' @md
#' @export
has_proper_calibration_range <- function(x_se) {
  min_c_conc <- calibration_min_pts(x_se)
  max_c_conc <- calibration_max_pts(x_se)
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

#' Replace values outside the concentration range with NA
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
    min_conc = calibration_min_pts(x),
    max_conc = calibration_max_pts(x)
  )
  x
}


## Calibration curve fitting ----------

#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param signal A vector of signal values
#' @param weight_method The method to use for weighting the models. The options are:
#'   - `largestR2`: The model with the largest R2 value is used as the weight
#'   - `1`: Constant weight of 1
#'   - `1/x`: Inverse of the concentration
#'   - `1/x2`: Inverse of the concentration squared
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
fit_and_test_calcurve_model <- function(conc,
                                        signal,
                                        weight_method = "largestR2",
                                        penalty_quadratic = 0) {
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
  # Limit the weight alternatives when one has been chosen
  if (weight_method != "largestR2") {
    weights_alt <- weights_alt[weight_method]
  }
  
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

### Post calibration ----------

#' Replace the values below the LLOQ and LOD
#' 
#' @param conc A matrix of concentrations
#' @param limits A data frame of the LLOQ and LOD. The names of the columns in `limits` should 
#'   have `lloq` and `lod`.
#' 
#' @returns A matrix with the values below the LLOQ and LOD replaced
#' @md
#' @export
replace_below_lod_lloq <- function(conc, limits) {
  stopifnot(nrow(conc) == nrow(limits))
  lab <- labelled::get_label_attribute(conc)
  out <- conc
  # Replace the values below LLOQ with half of the LLOQ
  out <- ifelse(conc < limits$lloq, limits$lloq / 2, out)
  # Replace the values below LOD with 1/4 of the LLOQ
  out <- ifelse(conc < limits$lod, limits$lloq / 4, out)
  labelled::set_label_attribute(out, lab)
}


