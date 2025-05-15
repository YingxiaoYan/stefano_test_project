# ------------------------------------------------------------------------------------------- #
# * Functions to access the elements of the MS-Dial data
# * Utility functions
# ------------------------------------------------------------------------------------------- #

# Variables used in `read-msdial.R` ------------------------------------------------------

## Features ----------

#' Feature variables
#' @export
conv_tbl <- tibble::tribble(
  ~given_id,         ~id,            ~label,
  "Alignment ID",    "alignment_id", "Alignment ID",
  "Metabolite name", "feature_name", "Feature Name",
  "Average Mz",      "mz",           "Average M/Z",
  "Quant mass",      "mz",           "Quant Mass",
  "Average Rt(min)", ".rt",          "Average Retention Time (min)",
  "S/N average",     "sn_ratio",     "Average S/N Ratio",
  "Comment",         ".std_type",    "Standard Type", # e.g. "Quant", "IS", "vIS", or NA
)

#' Get the retention time of the calibration curve samples
#'
#' @param x_se A [`SumExp::SumExp`] object
#' @returns A numeric vector of the retention times
#' @md
#' @export
retention_time <- function(x_se) {
  stopifnot(inherits(x_se, "SumExp"))
  SumExp::row_df(x_se)[[".rt"]]
}

#' Get the standard type of the features
#'
#' @param x_se A [`SumExp::SumExp`] object
#' @returns A character vector of the standard types
#' @export
std_type <- function(x_se) {
  stopifnot(inherits(x_se, "SumExp"))
  SumExp::row_df(x_se)[[".std_type"]]
}

#' Get if the features are targeted features
#'
#' @param x A [`SumExp::SumExp`] object or a character vector of standard types
#' @param ... Not used
#'
#' @returns A logical vector indicating whether the feature is a targeted feature
#' @export
is_targeted_feature <- function(x, ...) {
  UseMethod("is_targeted_feature")
}
#' @rdname is_targeted_feature
#' @export
is_targeted_feature.character <- function(x) {
  stopifnot(inherits(x, "character"))
  x == "Quant" | grepl("^Quant\\\\", x)
}
#' @rdname is_targeted_feature
#' @export
is_targeted_feature.SumExp <- function(x) {
  std_type(x) |>
    is_targeted_feature.character()
}

#' Get if the features are internal standards
#'
#' @param x A [`SumExp::SumExp`] object or a character vector of standard types
#' @param ... Not used
#' @returns A logical vector indicating whether the feature is an internal standard
#' @export
is_internal_std <- function(x, ...) {
  UseMethod("is_internal_std")
}
#' @rdname is_internal_std
is_internal_std.character <- function(x) {
  stopifnot(inherits(x, "character"))
  x == "IS" | grepl("^IS\\\\", x)
}
#' @rdname is_internal_std
is_internal_std.SumExp <- function(x) {
  std_type(x) |>
    is_internal_std.character()
}

## Samples -----------

#' The name of the column containing the spiked concentration points
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
avg_plus_std_times <- function(v, times, na.rm = TRUE) { # nolint
  s <- stats::sd(v, na.rm = na.rm)
  m <- mean(v, na.rm = na.rm)
  s * times + m
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @returns A numeric value of RSD%
.rsd_perc <- function(x, na.rm = FALSE) { # nolint
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
