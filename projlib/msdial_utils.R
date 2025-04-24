
# Variables used in `read-msdial.R` ------------------------------------------------------

#' Get the name of the element containing the spiked concentration points
#' @export
spiked_conc_pts_name <- "c_conc"

#' Get the spiked concentration points
#' 
#' @param cc_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @returns A numeric vector of the concentrations of the calibration curve samples
#' @md
#' @export
spiked_conc_pts <- function(cc_se) {
  stopifnot(inherits(cc_se, "SumExp"))
  SumExp::col_df(cc_se)[[spiked_conc_pts_name]]
}

#' Special control sample categories
#'
#' @param x_se A [`SumExp::SumExp`] object
#' @returns 
#' [ctrl_smpl_cat()]: A character vector of the control sample categories. 
#' 
#' @md
#' @export
ctrl_smpl_cat <- function(x_se) {
  stopifnot(inherits(x_se, "SumExp"))
  SumExp::col_df(x_se)[[".ctrl_cat"]]
}
#' @rdname ctrl_smpl_cat
#' @description
#' [exclude_ctrl_smpl_cat()]: Exclude one or more special control sample categories
#' 
#' @param cat A character vector of the control sample categories to be excluded
#' @returns
#' [exclude_ctrl_smpl_cat()]: A [`SumExp::SumExp`] object with the specified control sample
#' categories excluded
#' @md
#' @export
exclude_ctrl_smpl_cat <- function(x_se, cat) {
  stopifnot(is.character(cat))
  cat <- unique(cat)
  if (length(cat) == 0) {
    return(x_se)
  }
  x_se[, ! ctrl_smpl_cat(x_se) %in% cat]
}
#' @rdname ctrl_smpl_cat
#' @description
#' [extract_ctrl_smpl_cat()]: Extract one or more special control sample categories
#' @returns
#' [extract_ctrl_smpl_cat()]: A [`SumExp::SumExp`] object with the specified control sample
#' categories extracted
#' @md
#' @export
extract_ctrl_smpl_cat <- function(x_se, cat) {
  stopifnot(is.character(cat))
  cat <- unique(cat)
  if (length(cat) == 0) {
    return(x_se)
  }
  x_se[, ctrl_smpl_cat(x_se) %in% cat]
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
  g <- ifelse(ctrl_smpl_cat(x_se) == "CalCurve", out_names[1], out_names[2])
  SumExp::split_columns(x_se, g)
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

