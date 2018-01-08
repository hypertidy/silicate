#' Unique edges for arc-node topology. 
#' 
#' So-called "arcs" are unclosed paths that end in nodes, vertices shared by other arcs. 
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