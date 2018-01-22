
#' Path decomposition
#' 
#' Start in the middle, and build the 'path-link-vertex' table. 
#'
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_path
#' @export
#' @seealso `sc_coord` for the coordinates part of the model, `sc_object` for 
#' the features, and `PATH` for the full model. 
sc_path <- function(x, ...) {
  UseMethod("sc_path")
}
#' @name sc_path
#' @export
sc_path.default <- function(x, ...) {
  PATH(x)[["path"]]
}
#' @name sc_path
#' @export
sc_path.PATH <- function(x, ...) {
  x[["path"]]
}
#' @name sc_path
#' @export
sc_path.ARC <- function(x, ...) {
  stop("sc_path not yet supported for ARC")
}
#' @name sc_path
#' @export
sc_path.SC <- function(x, ...) {
  stop("sc_path not yet supported for SC")
}