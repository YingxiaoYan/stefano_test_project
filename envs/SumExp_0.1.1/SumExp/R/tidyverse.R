#' @include SumExp-class.R
NULL

#' Coerce a SumExp object to a `tibble`
#'
#' @param x A [`SumExp`] object
#' @param ... Not used
#' @return A `tibble` with the row and column data frames as well as the matrices. Each matrix
#'   in the [`SumExp`] object is represented as a long format tibble with columns `.row_id`,
#'   `.col_id`, and the matrix name.
#' @export
setGeneric("as_tibble", function(x, ...) standardGeneric("as_tibble"))
#' @rdname as_tibble
#' @export
setMethod(
  "as_tibble", signature(x = "SumExp"),
  function(x, ...) {
    col_tbl <- tibble::as_tibble(x@col_df, rownames = ".col_id")
    row_tbl <- tibble::as_tibble(x@row_df, rownames = ".row_id")

    mx <- lapply(names(x), \(ii) {
      out <- tibble::as_tibble(x[[ii]], rownames = ".row_id") |>
        tidyr::pivot_longer(cols = -c(.row_id), names_to = ".col_id", values_to = ii)
    })
    .join <- function(x, y) dplyr::full_join(x, y, by = c(".row_id", ".col_id"))
    mx <- Reduce(.join, mx)
    # Copy the label attributes
    for(ii in names(mx)[names(mx) %in% names(x)]) {
      labelled::label_attribute(mx[[ii]]) <- labelled::label_attribute(x[[ii]])
    }

    mx |>
      dplyr::left_join(row_tbl, by = ".row_id") |>
      dplyr::left_join(col_tbl, by = ".col_id")
  }
)

#' Create a ggplot object from a SumExp object
#'
#' @param data A [`SumExp`] object
#' @inheritParams ggplot2::ggplot
#' @return A ggplot object
#' @export
setGeneric(
  "ggplot",
  function(data, mapping = ggplot2::aes(), ..., environment = parent.frame()) {
    standardGeneric("ggplot")
  }
)
#' @rdname ggplot
#' @export
setMethod(
  "ggplot", signature(data = "SumExp"),
  function(data, mapping = ggplot2::aes(), ..., environment = parent.frame()) {
    data |>
      as_tibble() |>
      ggplot2::ggplot(mapping = mapping, ..., environment = environment)
  }
)
