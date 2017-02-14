
#' Coordinate decomposition
#' 
#' Collect all coordinates in one table, the path_link_vertex table
#' has the information about the original grouping.  
#' 
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_coord
#' @export
#' @seealso `sc_path` for the central part of the model, `sc_object` for 
#' the features, and `PATH` for the full model. 
sc_coord <- function(x, ...) UseMethod("sc_coord")

