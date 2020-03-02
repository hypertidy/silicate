#' Nodes for arc-node topology.
#'
#' Nodes are the vertices in the graph that are shared by "arcs".
#'
#' @param x input object
#' @param ... arguments for methods
#' @return data frame of the nodes
#' @export
#' @importFrom rlang .data
#' @examples
#' sc_node(ARC(minimal_mesh))
sc_node <- function(x, ...) {
  UseMethod("sc_node")
}
#' @name sc_node
#' @export
#' @examples
#' sc <- SC(routes)
#' library(dplyr)
#' plot(sc)
#' sc_node(sc) %>% inner_join(sc$vertex) %>% select(x_, y_) %>% points()
sc_node.SC <- function(x, ...) {
  alledge <- tibble::tibble(v = c(x$edge$.vx0, x$edge$.vx1),
                            e = c(x$edge$edge_, x$edge$edge_))
  v0 <- alledge %>% group_by(.data$v) %>% tally() %>% filter(!n == 2)
  tibble::tibble(vertex_ = sort(unique(v0$v)))
}
#' @name sc_node
#' @export
sc_node.SC0 <- function(x, ...) {
  sc <- SC(x)
  nodes_id <- sc_node(sc)
  nodes_id %>%
    dplyr::inner_join(sc$vertex, "vertex_") %>%
    dplyr::select(-.data$vertex_)
}
#' @name sc_node
#' @export
sc_node.default <- function(x, ...) {
  x <- SC(x)
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
  x$arc_link_vertex %>%
    dplyr::add_count(.data$vertex_) %>%
    dplyr::filter(.data$n > 2) %>%
    dplyr::distinct(.data$vertex_)
}

sc_node_base <- function(unique_edges, vertex, ...) {
  nodes <- dplyr::bind_rows(vertex %>% dplyr::select(.data$vertex_) %>%
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vx0")),
                            vertex %>% dplyr::select(.data$vertex_) %>%
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vx1"))) %>%
    dplyr::distinct(.data$edge_, .data$vertex_) %>%
    dplyr::group_by(.data$vertex_) %>% dplyr::tally() %>% dplyr::ungroup() %>%
    dplyr::filter(.data$n > 2) %>% dplyr::distinct(.data$vertex_)
  nodes
}
