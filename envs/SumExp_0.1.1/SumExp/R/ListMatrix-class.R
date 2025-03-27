#' A class for storing a list of matrices
#'
#' @description
#' This class inherits [`base::list`] class and is used to store a list of matrices. This class
#' makes sure that all matrices are named and have the same dimensions and row/column names.
#'
#' @slot .Data A list with matrices. All matrices must have the same dimensions and row/column
#'   names.
#'
#' @param x,object A [`ListMatrix`] object
#' @param i An integer or a character
#' @param value A matrix for `[[<-`;
#'   A character vector for `colnames<-` and `rownames<-`;
#'   A list of row and column names for `dimnames<-`
#' @examples
#' nms <- list(LETTERS[1:4], letters[1:5])
#' m1 <- matrix(sample(20), nrow = 4, dimnames = nms)
#' m2 <- matrix(sample(LETTERS, 20), nrow = 4, dimnames = nms)
#' l <- ListMatrix('mat1' = m1, 'mat2' = m2)
#'
#' @importFrom methods setClass setMethod setValidity setGeneric callNextMethod validObject
#'   signature new show
#' @exportClass ListMatrix
#' @export
ListMatrix <- setClass(
  "ListMatrix",
  contains = "list"
)

setMethod(
  "initialize", "ListMatrix",
  function(.Object, ...) {
    .Object@.Data <- list(...)
    validObject(.Object)
    return(.Object)
  }
)

setValidity("ListMatrix", function(object) {
  for(ii in seq_along(object)) {
    mat_i <- object[[ii]]
    if (!is.matrix(mat_i)) return("Every element must be a matrix")
    if (ncol(mat_i) > 0 & is.null(colnames(mat_i))) {      # Allow matrices with no columns
      return("Column names in all matrices must be defined")
    }
    if (nrow(mat_i) > 0 & is.null(rownames(mat_i))) {      # Allow matrices with no rows
      return("Row names in all matrices must be defined")
    }
    if (ii > 1) {       # Check identity across matrices
      mat_i1 <- object[[ii - 1]]
      if (!identical(dim(mat_i), dim(mat_i1))) {
        return("Dimensions of all matrices must be equal")
      }
      if (any(colnames(mat_i) != colnames(mat_i1))) {
        return("Column names in all matrices must be equal")
      }
      if (any(rownames(mat_i) != rownames(mat_i1))) {
        return("Row names in all matrices must be equal")
      }
    }
  }
  TRUE
})

# Methods --------------------------------------------------------------------------------

#' @rdname ListMatrix-class
#' @examples
#' # Get as list
#' x <- as.list(l)
#' str(x)
#' @export
setMethod("as.list", signature(x = "ListMatrix"), function(x) {
  stats::setNames(x@.Data, nm = names(x))
})
#' @rdname ListMatrix-class
#' @examples
#' # Update a matrix in a ListMatrix object
#' m3 <- m2
#' m3[2, ] <- c(letters[5:9])
#' l[[2]] <- m3
#' l
#' @export
setMethod("[[<-" , signature(x = "ListMatrix"), function(x, i, value) {
  x <- callNextMethod()
  validObject(x)
  x
})

#' @rdname ListMatrix-class
#' @examples
#' # Get the dimensions of a ListMatrix object
#' dim(l)
#' @export
setMethod("dim", signature(x = "ListMatrix"), function(x) {
  if (length(x) == 0) return(c(0, 0))
  dim(x[[1]])
})

# No generic in `base`. But, the function exists there with the arguments.
setGeneric("nrow", function(x) standardGeneric("nrow"))
#' @rdname ListMatrix-class
#' @examples
#' nrow(l)
#' @export
setMethod("nrow", signature(x = "ListMatrix"), function(x) dim(x)[1])

# No generic in `base`. But, the function exists there with the arguments.
setGeneric("ncol", function(x) standardGeneric("ncol"))
#' @rdname ListMatrix-class
#' @examples
#' ncol(l)
#' @export
setMethod("ncol", signature(x = "ListMatrix"), function(x) dim(x)[2])

#' @rdname ListMatrix-class
#' @export
setMethod("dimnames", signature(x = "ListMatrix"), function(x) {
  if (length(x) == 0) return(list(NULL, NULL))
  dimnames(x[[1]])
})
#' @rdname ListMatrix-class
#' @export
setMethod("dimnames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    dimnames(x[[ii]]) <- value
  }
  x
})

# No generic in `base`. But, the function exists there with the arguments.
setGeneric("colnames", function(x, do.NULL = TRUE, prefix = "col") standardGeneric("colnames"))
#' @rdname ListMatrix-class
#' @export
setMethod("colnames", signature(x = "ListMatrix"), function(x) dimnames(x)[[2]])
setGeneric("colnames<-", function(x, value) standardGeneric("colnames<-"))
#' @rdname ListMatrix-class
#' @export
setMethod("colnames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    colnames(x[[ii]]) <- value
  }
  x
})

# No generic in `base`. But, the function exists there with the arguments.
setGeneric("rownames", function(x, do.NULL = TRUE, prefix = "row") standardGeneric("rownames"))
#' @rdname ListMatrix-class
#' @export
setMethod("rownames", signature(x = "ListMatrix"), function(x) dimnames(x)[[1]])
setGeneric("rownames<-", function(x, value) standardGeneric("rownames<-"))
#' @rdname ListMatrix-class
#' @export
setMethod("rownames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    rownames(x[[ii]]) <- value
  }
  x
})

#' @rdname ListMatrix-class
#' @export
setMethod("show", signature(object = "ListMatrix"), function(object) {
  d <- dim(object)
  cat("ListMatrix with", length(object), "matrices with dimension of", d)
  for(ii in seq_along(object)) {
    nm <- names(object)[ii]
    if (!is.null(nm)) nm <- paste0("`", nm, "`")
    cat("\nMatrix", ii, nm, ":\n")
    print(object[[ii]][0:min(d[1L], 5), 0:min(d[2L], 5)])   # Maximum 5 rows and columns
  }
})
#' @rdname ListMatrix-class
#' @export
setMethod("print", signature(x = "ListMatrix"), function(x) show(x))




