#' Objects, features
#' 
#' The objects are the front end entities, the usual "GIS contract" objects, 
#' the features. 
#' 
#' @seealso `sc_coord` for the coordinates part of the model, `sc_path` for 
#' the central part of the model, and `PATH` for the full model. 
#' @name sc_object
#' @importFrom tibble as_tibble
#' @export
#' @examples 
#' #library(sf)
#' #nc <-  st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' #sc_object(nc)
sc_object.sf <- function(x, ...) {
  faster_as_tibble(.st_set_geometry(x))
}

#' @name sc_object
#' @export
sc_object.sfc <- function(x, ...) {
  tibble(object_ = sc_uid(length(x)))
}


## a function sf should have
## to drop the spatial stuff
.st_set_geometry <- function(x, value = NULL) {
  #st_geometry(x) <- value
  x[[attr(x, "sf_column")]] <- NULL
  as.data.frame(x)
}

.st_get_geometry <- function(x) {
  x[[attr(x, "sf_column")]]
}