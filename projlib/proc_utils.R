#' Compute the average plus the standard deviation multiplied by `times`
#'
#' @param v A numeric vector
#' @param times Multiplication factor to the standard deviation.
#' @param na.rm A logical value indicating whether to remove NA values
#'
#' @returns A numeric vector of the average plus the standard deviation multiplied by `times` 
#' @md
#' @export
avg_plus_std_times <- function(v, times, na.rm = TRUE) {
  s <- stats::sd(v, na.rm = na.rm)
  m <- mean(v, na.rm = na.rm)
  s * times + m
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @returns A numeric value of RSD%
.rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Get the calibration curve concentrations
#' 
#' @param cc_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @returns A numeric vector of the concentrations of the calibration curve samples
#' @md
#' @export
concentration_points <- function(cc_se) {
  stopifnot("SPIKED_CONC" %in% names(SumExp::metadata(cc_se)))
  sc <- SumExp::metadata(cc_se)$SPIKED_CONC
  SumExp::col_df(cc_se)[[sc]]
}

#' Split a matrix column-wise by the values
#'
#' @param x A matrix or a data frame to be split
#' @param value A numeric vector of the values to split the matrix. The length of the vector
#'   should be the same as the number of columns of `x`.
#'   A common value is the concentration points, the output of [concentration_points()].
#'
#' @returns A list of matrices split by the values column-wise. The list is sorted by the
#'   values.
#' @md
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
#' @md
get_min_value_satisfy_cond <- function(cond, values = names(cond), shift = 0) {
  values <- as.numeric(values)
  stopifnot("`values` are not sorted" = identical(sort(values), values))   # Already sorted 
  if (all(!cond)) return(NA_real_)
  ii <- min(which(cond))
  values[ii + shift]
}
