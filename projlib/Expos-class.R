box::use(SumExp)

#' A class for calibration curve samples' data
#'
#' @description
#' This class inherits [`SumExp::SumExp`] class and enhances it with additional information for
#' calibration curves.
#' 
#' @slot spiked A matrix of the spiked values, likely to be used for calibration.
#'
#' @examples
#' nms <- list(LETTERS[1:4], letters[1:5])
#' m1 <- matrix(sample(20), nrow = 4, dimnames = nms)
#' m2 <- matrix(sample(LETTERS, 20), nrow = 4, dimnames = nms)
#' df_c <- data.frame(
#'   x = c("alpha", "beta", "gamma", "delta", "epsilon"),
#'   type = gl(2, 3, 5, c("", "fruit")),
#'   row.names = nms[[2]]
#' )
#' df_r <- data.frame(
#'   y = rnorm(4),
#'   grp = rep(c("White", "Black"), each = 2),
#'   row.names = nms[[1]]
#' )
#' spiked <- matrix(rep(c(0.01, 0.1, 1, 10, 100), each = 4), nrow = 4, dimnames = nms)
#' cc <- Expos(a = m1, b = m2, row_df = df_r, col_df = df_c, spiked = spiked)
#'
#' @importFrom methods setClass setMethod setValidity setGeneric callNextMethod validObject
#'   signature new show
#' @importClassesFrom SumExp SumExp
#' @exportClass Expos
#' @md
#' @export
Expos <- setClass(
  "Expos",
  contains = "SumExp",
  slots = c(
    spiked = "matrix"
  )
)

setMethod(
  "initialize",
  signature = "Expos",
  definition = function(.Object,
                        ...,
                        spiked = matrix(),
                        col_df = data.frame(),
                        row_df = data.frame(),
                        metadata = list()) {
    .Object@spiked <- spiked
    callNextMethod(.Object, ..., col_df = col_df, row_df = row_df, metadata = metadata)
  }
)

setValidity("Expos", function(obj) {
  if (!is.matrix(obj@spiked)) {
    return("`@spiked` must be a matrix")
  }
  if (length(obj) > 0 & any(dim(obj@spiked) != dim(obj@.Data[[1]]))) {
    return("Spiked value matrix must have the same dimensions as the data matrix")
  }
  TRUE
})

# Accessors ------------------------------------------------------------------------------

#' Accessors for [`Expos`] class objects
#'
#' @param cc_se A [`Expos`] object
#' @rdname Expos-class
#' @md
#' @export
setGeneric("spiked", function(cc_se) standardGeneric("spiked"))
#' @rdname Expos-class
setMethod("spiked", "Expos", function(cc_se) cc_se@spiked)

#' Convert a SumExp object to a Expos object
#'
#' @param x A [`SumExp::SumExp`] object
#' @param spiked A matrix of spiked values
#' @param ... Additional arguments
#'
#' @returns A [`Expos`] object
#' @md
#' @examples
#' se <- SumExp::exmpl_se
#' print(se)
#' spiked <- matrix(rep(c(0.01, 0.1, 1, 10, 100), each = 4), nrow = 4, dimnames = nms)
#' cc <- as_Expos(se, spiked = spiked)
#' print(cc)
#' @export
as_Expos <- function(x, spiked, ...) {
  UseMethod("as_Expos")
}
#' @rdname as_Expos
as_Expos.SumExp <- function(x, spiked) {
  lst_mat <- SumExp::as.list(x)
  row_df <- SumExp::row_df(x)
  col_df <- SumExp::col_df(x)
  metadata <- SumExp::metadata(x)
  do.call("new", c(
    lst_mat, list(Class = "Expos", row_df = row_df, col_df = col_df, metadata = metadata)
  ))
}

# Methods --------------------------------------------------------------------------------

