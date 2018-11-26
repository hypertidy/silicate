add_rownum <- function(x, name) {
  x[[name]] <- seq_len(nrow(x))
  x
}

#' Edges.
#'
#' Simple binary relationships, a primitive composed of two vertices.
#'
#' Edges are unique, undirected line segments. Compare to `sc_segment` which refers to all
#' instances of edges.
#'
#' `sc_start` and `sc_end` are convenience functions that provide the obvious
#' start and end coordinates by joining on the appropriate edge vertex label, `.vertex0`
#' or `.vertex1`. Currently this returns the ordered segments, along with their unique (unordered) `edge_`, as
#' well as unique `segment`, a `object_` labels.
#' @param x input object
#' @param ... arguments for methods
#' @name sc_edge
#' @aliases sc_start sc_end
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
#' @export
sc_edge.TRI <- function(x, ...) {
  SC(x)[["edge"]]
}

#' @export
sc_edge.SC0 <- function(x, ...) {
 tidyr::unnest(x$object["topology_"], .id = "object_")
}

#' @name sc_edge
#' @export
sc_edge.PATH <- function(x, ...) {
  sc_segment(x, ...) %>% dplyr::distinct(.data$edge_, .keep_all = TRUE) %>%
    dplyr::select(.data$.vertex0, .data$.vertex1, .data$edge_)
}

#' @name sc_edge
#' @export
sc_start <- function(x, ...) {
  UseMethod("sc_start")
}
#' @name sc_edge
#' @export
sc_start.SC <- function(x, ...) {
  sc_edge(x, ...) %>% dplyr::inner_join(sc_vertex(x), c(".vx0" = "vertex_")) %>%
    dplyr::select(.data$x_, .data$y_)
}
#' @name sc_edge
#' @export
sc_start.SC0 <- function(x, ...) {
  sc_edge(x, ...) %>% dplyr::inner_join(sc_vertex(x)  %>% add_rownum("vertex_"), c(".vx0" = "vertex_")) %>%
    dplyr::select(.data$x_, .data$y_)
}

#' @name sc_edge
#' @export
sc_start.PATH <- function(x, ...) {
  sc_start(SC(x), ...)
}
#' @name sc_edge
#' @export
sc_start.ARC <- function(x, ...) {
  sc_start(SC(x), ...)
}
#' @name sc_edge
#' @export
sc_start.TRI <- function(x, ...) {
  sc_start(SC(x), ...)
}

#' @name sc_edge
#' @export
sc_end <- function(x, ...) {
  UseMethod("sc_end")
}
#' @name sc_edge
#' @export
sc_end.SC <- function(x, ...) {
  ## FIXME: we aren't giving out edges per object (see also sc_coord.SC)
  sc_edge(x, ...) %>% dplyr::inner_join(sc_vertex(x), c(".vx1" = "vertex_")) %>%
  dplyr::select(.data$x_, .data$y_)
}
#' @name sc_edge
#' @export
sc_end.SC0 <- function(x, ...) {
  ## FIXME: we aren't giving out edges per object (see also sc_coord.SC)
  sc_edge(x, ...) %>% dplyr::inner_join(sc_vertex(x)  %>% add_rownum("vertex_"), c(".vx1" = "vertex_")) %>%
    dplyr::select(.data$x_, .data$y_)
}

#' @name sc_edge
#' @export
sc_end.PATH <- function(x, ...) {
  sc_end(SC(x), ...)
}
#' @name sc_edge
#' @export
sc_end.ARC <- function(x, ...) {
  sc_end(SC(x), ...)
}
#' @name sc_edge
#' @export
sc_end.TRI <- function(x, ...) {
  sc_end(SC(x), ...)
}
