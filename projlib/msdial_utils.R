
# Variables used in `read-msdial.R` ------------------------------------------------------

#' Get the spiked concentration points
#' 
#' @param cc_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @returns A numeric vector of the concentrations of the calibration curve samples
#' @md
#' @export
spiked_conc_pts <- function(cc_se) {
  SumExp::col_df(cc_se)[["c_conc"]]
}

#' Special control sample categories
#'
#' @param x_se A [`SumExp::SumExp`] object
#' @returns A character vector of the control sample categories. 
#' @md
#' @export
contr_cat <- function(x_se) {
  SumExp::col_df(x_se)[["contr_cat"]]
}

#' Split the columns of a [`SumExp::SumExp`] object into the calibration curve and the other
#'
#' @param x_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @param out_names A character vector of length 2. The names of the output list.
#'   The first element is the name of the calibration curve samples.
#' @returns A list of two [`SumExp::SumExp`] objects with the names `CalCurve` and `Other`
#' @md
#' @export
split_into_calcurve_and_other <- function(x_se, out_names = c("CalCurve", "Other")) {
  stopifnot(length(out_names) == 2)
  g <- ifelse(contr_cat(x_se) == "CalCurve", out_names[1], out_names[2])
  SumExp::split_columns(x_se, g)
}

#' Extract the calibration curve samples
#'
#' @param x_se A [`SumExp::SumExp`] object including the calibration curve samples
#'
#' @returns A [`SumExp::SumExp`] object of the calibration curve samples
#' @md
#' @export
extract_calcurve <- function(x_se) {
  x_se[, contr_cat(x_se) == "CalCurve"]
}


# Utils ----------------------------------------------------------------------------------

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

#' Get the min-/max-imum value that satisfies the condition
#' 
#' @param cond A logical value to satisfy.
#' @param values A numeric or character vector of values that are already sorted. 
#'   If it is not given, the names of `cond` are used. 
#'   When it is a character vector, the values are converted to numeric.
#' 
#' @returns A value from the `values` that satisfies the condition `cond`. 
#'   If no value satisfies the condition, return `NA`.
#' @md
get_values_satisfy <- function(cond, values = names(cond)) {
  values <- as.numeric(values)
  stopifnot("`values` are not sorted" = identical(sort(values), values))   # Already sorted 
  if (all(!cond)) return(NA_integer_)
  values[which(cond)]
}
#' @rdname get_values_satisfy
#' @aliases get_min_satisfy
#' @examples
#' get_min_satisfy(c(TRUE, FALSE, TRUE), c(5:7))
#' get_min_satisfy(c("4" = FALSE, "5" = TRUE, "6" = TRUE))
#' get_min_satisfy(c(1:3) > 1, c(5:7)) 
#' get_min_satisfy(c(1:3) > 3, c(5:7)) 
#' \dontrun{
#' get_min_satisfy(c(1:3) > 1, c(5, 3, 9))        # Error because not sorted
#' }
#' @rdname get_values_satisfy
#' @export
get_min_satisfy <- function(cond, values = names(cond)) {
  get_values_satisfy(cond, values)[1L]
}
#' @rdname get_values_satisfy
#' @examples
#' get_max_satisfy(c(TRUE, FALSE, TRUE), c(5:7))
#' get_max_satisfy(c("4" = FALSE, "5" = TRUE, "6" = TRUE))
#' get_max_satisfy(c(1:3) < 2, c(5:7)) 
#' get_max_satisfy(c(1:3) > 3, c(5:7)) 
#' @export
get_max_satisfy <- function(cond, values = names(cond)) {
  out <- get_values_satisfy(cond, values)
  out[length(out)]
}
