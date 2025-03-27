#' Copy labels handling nested lists
#'
#' [labelled::copy_labels()] does not handle nested lists. This function is a workaround.
#' @inheritParams labelled::copy_labels
copy_labels_list <- function(from, to) {
  labelled::label_attribute(to) <- labelled::label_attribute(from)
  if (length(from) == 0 || length(to) == 0) return(to)
  if (!is.list(from)) {
    if (is.vector(from)) {
      # labelled::copy_labels() accepts only a vector or a data frame
      return(labelled::copy_labels(from = from, to = to))
    } else {
      return(to)
    }
  }
  lapply(1:length(from), \(ii) {
    # Recursively copy labels through lists
    copy_labels_list(from[[ii]], to[[ii]])
  })
}

#' Subset and copy labels handling nested lists
#'
#' @param from A data frame
#' @param idx Index to subset `from`
subset_copy_labels.data.frame <- function(from, idx) {
  out <- from[idx, , drop = FALSE]
  # Copy labels
  for(ii in 1:ncol(out)) {
    # [labelled::copy_labels()] does not handle nested lists.
    labelled::label_attribute(out[[ii]]) <- labelled::label_attribute(from[[ii]])
  }
  out
}

