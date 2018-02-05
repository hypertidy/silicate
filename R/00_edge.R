#' Edges. 
#' 
#' Simple binary relationships, a primitive composed of two vertices. 
#' 
#' Edges are unique, undirected line segments. Compare to `sc_segment` which refers to all
#' instances of edges. 
#' @param x input object
#' @param ... arguments for methods
#' @name sc_edge
#' @export
sc_edge <- function(x, ...) {
  UseMethod("sc_edge")
}
#' @name sc_edge
#' @export
sc_edge.default <- function(x, ...) {
  x <- PATH(x, ...)
  sc_edge(x)
}
#' @export
sc_edge.SC <- function(x, ...) {
  x[["edge"]]
}
#' @name sc_edge
#' @export
sc_edge.PATH <- function(x, ...) {
  sc_segment(x, ...) %>% dplyr::distinct(.data$edge_, .keep_all = TRUE) %>% 
    dplyr::select(.data$.vertex0, .data$.vertex1, .data$edge_)
}