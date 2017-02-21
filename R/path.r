
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
sc_path.PATH <- function(x, ...) {
  x[["path"]]
}
