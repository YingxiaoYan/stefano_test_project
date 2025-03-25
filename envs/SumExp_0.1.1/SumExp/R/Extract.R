#' @include SumExp-class.R
NULL

#' Extract a part from a [`SumExp`] object
#'
#' @param x A [`SumExp`] object
#' @param i,j Indices specifying elements to extract. The indices can be numeric, character or
#'   logical vectors. If given as a quoted expression (ie. [`call`] or [`quosure`] object), it
#'   will be evaluated in the context of the [`SumExp`] object. The `i` is evaluated in the
#'   context of `row_df(x)` and `j` in the context of `col_df(x)`.
#' @param ... Not used
#' @param drop Not supported. Always `FALSE`
#'
#' @export
setMethod(
  "[", signature(x = "SumExp"),
  function(x, i, j, ..., drop = FALSE) {
    .get_ij <- function(ij, df) {
      if (missing(ij)) {
        rep_len(TRUE, nrow(df))
      } else if (is.call(ij) | rlang::is_quosure(ij)) {    # Quoted expression
        eval(ij, df, parent.frame())
      } else {
        ij
      }
    }
    i <- .get_ij(i, x@row_df)
    j <- .get_ij(j, x@col_df)
    stopifnot("`drop` is not supported" = !drop)

    data_lst <- lapply(x, \(.x) {
      .x[i, j, drop = FALSE] |>
        labelled::copy_labels_from(.x)
    })

    col_df <- subset_copy_labels.data.frame(x@col_df, j)
    row_df <- subset_copy_labels.data.frame(x@row_df, i)
    metadata <- x@metadata
    do.call("new", c(
      data_lst,
      list(Class = "SumExp", col_df = col_df, row_df = row_df, metadata = metadata)
    ))
  }
)
