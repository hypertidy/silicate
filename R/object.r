#' Objects, features
#' 
#' The objects are the front end entities, the usual "GIS contract" objects, 
#' or features. 
#' 
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_object
#' @export
#' @seealso `sc_coord` for the coordinates part of the model, `sc_path` for 
#' the central part of the model, and `PATH` for the full model. 
sc_object <- function(x, ...) UseMethod("sc_object")

#' @name sc_object
#' @export
sc_object.default <- function(x, ...) {
  as_tibble(x[["object"]])
}