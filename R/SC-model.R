## PATH() sc_coord, sc_path, sc_object,

## SC() sc_vertex, sc_edge, sc_segment
#' @examples
#' dd <- minimal_mesh
#' #dd <- nc
#' #dd <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
#' x <- SC(dd)
#' plot(x)
#' xt <- triangulate.SC(x, D = TRUE)
#' plot(xt)
triangulate.SC <- function(x, ...) {
  v <- x$vertex
  a <- match(x$edge$.vertex0, v$vertex_)
  b <- match(x$edge$.vertex1, v$vertex_)
  p <- RTriangle::pslg(as.matrix(dplyr::select(v, x_, y_)), 
                       S = cbind(a, b))
  t <- RTriangle::triangulate(p, ...)
  structure(list(TRI = t$T, V = t$P), class = "TRI")
}
plot.TRI <- function(x, ...) {
  plot(x$V, pch = ".")
  idx <- t(cbind(x$TRI, NA))
  polygon(x$V[idx, ])
}
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