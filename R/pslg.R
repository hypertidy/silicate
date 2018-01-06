## PATH() sc_coord, sc_path, sc_object

## EDGE() sc_vertex, sc_edge, sc_segment

## this is the pure Planar Straight Line Graph model
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
sc_vertex.default <- function(x, ...) {
  EDGE(x)[["vertex"]]
}
sc_vertex.EDGE <- function(x, ...) {
  x[["vertex"]]
}
EDGE <- function(x, ...) {
  UseMethod("EDGE")
}

EDGE.default <- function(x, ...) {
  P <- PATH(x, ...)
  structure(list(object = sc_object(P), 
       edge = sc_edge(P), 
       vertex = sc_vertex(P)), 
       class = c("EDGE", "sc"))
}

plot.EDGE <- function(x, ...) {
  v <- sc_vertex(x)
  e <- sc_edge(x)
  x0 <- v %>% dplyr::inner_join(e, c(".vertex_" = ".vertex0"))
  x1 <- v %>% dplyr::inner_join(e, c(".vertex_" = ".vertex1"))
  
  ggplot2::ggplot()
}