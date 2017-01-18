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
#' @seealso `sc_coord` for the coordinates part of the model, `sc_branch` for 
#' the central part of the model, and `BRANCH` for the full model. 
sc_object <- function(x, ...) UseMethod("sc_object")

#' @name sc_object
#' @export
#' @examples 
#' library(sf)
#' nc <-  st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' sc_object(nc)
sc_object.sf <- function(x, ...) {
  as.data.frame(x)
}