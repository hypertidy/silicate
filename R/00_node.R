#' Nodes for arc-node topology. 
#' 
#' Nodes are the vertices in the graph that are shared by "arcs". 
#' 
#' @seealso NARC
#' @param x input object
#' @param ... arguments for methods
#' @export
sc_node <- function(x, ...) {
  UseMethod("sc_node")
}
#' @name sc_node
#' @export
sc_node.default <- function(x, ...) {
  x <- PATH(x)
  sc_node(x)
}
#' @name sc_node
#' @export
sc_node.PATH <- function(x, ...) {
  sc_node_base(sc_edge(x), x[["vertex"]])
}
#' @name sc_node
#' @export
sc_node.ARC <- function(x, ...) {
  ## remove all non-noded arcs
  x$arc_link_vertex %>% dplyr::add_count(vertex_) %>% dplyr::filter(n > 2) %>% dplyr::distinct(.data$vertex_)
}
#' @importFrom rlang .data
sc_node_base <- function(unique_edges, vertex, ...) {
  nodes <- dplyr::bind_rows(vertex %>% dplyr::select(.data$vertex_) %>% 
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                            vertex %>% dplyr::select(.data$vertex_) %>% 
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(.data$edge_, .data$vertex_) %>% 
    dplyr::group_by(.data$vertex_) %>% dplyr::tally() %>% dplyr::ungroup() %>% 
    dplyr::filter(.data$n > 2) %>% dplyr::distinct(.data$vertex_) 
  nodes
}
