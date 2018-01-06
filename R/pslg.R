## PATH() sc_coord, sc_path, sc_object

## SC() sc_vertex, sc_edge, sc_segment

## this is the pure Planar Straight Line Graph model
#' @export
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
#' @export
sc_vertex.default <- function(x, ...) {
  PATH(x)[["vertex"]]
}
#' @export
sc_vertex.SC <- function(x, ...) {
  x[["vertex"]]
}
#' @export
SC <- function(x, ...) {
  UseMethod("SC")
}
#' @export
SC.default <- function(x, ...) {
  P <- PATH(x, ...)
  O <- sc_object(P)
  E <- sc_edge(P)
  structure(list(object = O, 
       edge = E, 
       vertex = sc_vertex(P)), 
       class = c("SC", "sc"))
}

compact_labels <- function(x) {
  v0 <- as.integer(match(x$edge$.vertex0, x$vertex$vertex_))
  v1 <- as.integer(match(x$edge$.vertex1, x$vertex$vertex_))
  
  x$vertex$vertex_ <- NULL #(seq_len(nrow(x$vertex)))
  x$edge <- dplyr::mutate(x$edge, .vertex0 = v0, .vertex1 = v1, edge_ = seq_len(nrow(x$edge)))
  x
}
#' @export
plot.SC <- function(x, ...) {
  v <- sc_vertex(x)
  e <- sc_edge(x)
  x0 <- e %>% dplyr::inner_join(v, c(".vertex0" = "vertex_"))
  x1 <- e %>% dplyr::inner_join(v, c(".vertex1" = "vertex_"))
  plot(v$x_, v$y_, pch = ".")
  segments(x0$x_, x0$y_, x1$x_, x1$y_, ...)
}