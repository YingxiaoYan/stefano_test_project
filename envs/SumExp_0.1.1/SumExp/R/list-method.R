#' Get names and label attributes of a list
#'
#' @param x A list with names and optionally label attributes
#' @return A character vector with the names. If elements have label attributes by a method of
#'   [`labelled`] package, the labels are returned as the names of the returned character
#'   vector.
#'
#' @examples
#'   x <- list(a = 1:3, b = 4:6)
#'   labelled::var_label(x$a) <- "A"
#'   name_labs(x)
#' @export
name_labs <- function(x) {
  nms <- names(x)
  labs <- sapply(nms, \(.x) {
    l <- labelled::label_attribute(x[[.x]])
    if (is.null(l)) .x else l     # If no attribute, return the name
  })
  names(nms) <- labs
  nms
}
