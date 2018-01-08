
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

#' @name sc_coord
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join transmute_ select_
sc_coord.PATH <- function(x, ...) {
  dplyr::inner_join(dplyr::transmute(x[["path_link_vertex"]], vertex_ = .data$vertex_) , 
                    x[["vertex"]], "vertex_") %>% 
    dplyr::select(-.data$vertex_)
}