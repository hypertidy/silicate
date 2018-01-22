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

#' @export
SC <- function(x, ...) {
  UseMethod("SC")
}
#' @export
SC.default <- function(x, ...) {
  P <- PATH(x, ...)
  O <- sc_object(P)
  S <- sc_segment(P)
  E <- dplyr::select(S, .data$.vertex0, .data$.vertex1, .data$edge_) %>% 
    dplyr::distinct(.data$edge_, .keep_all = TRUE)
  ExO <- S %>% 
    dplyr::select(.data$path_, .data$edge_) %>% 
    dplyr::inner_join(dplyr::select(P[["path"]], .data$path_, .data$object_), "path_") %>% 
  dplyr::distinct(.data$edge_, .data$object_) 
  structure(list(object = O,
                 object_link_edge = ExO,
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
plot.SC <- function(x, ..., vars = NULL) {
  
  v <- sc_vertex(x)
  if (!is.null(vars)) {
    vars <- c(vars, "vertex_")
    v <- dplyr::select(v, vars) %>% 
      setNames(c("x_", "y_", "vertex_"))
  }
  e <- sc_edge(x)
  x0 <- e %>% dplyr::inner_join(v, c(".vertex0" = "vertex_"))
  x1 <- e %>% dplyr::inner_join(v, c(".vertex1" = "vertex_"))
  idx <- factor(x$object_link_edge$object_)[seq(1, nrow(e))]
  col <- rainbow(nlevels(idx))[idx]
  plot(v$x_, v$y_, pch = ".")
  segments(x0$x_, x0$y_, x1$x_, x1$y_, ..., col = col)
}

filter.SC <- function(x, ...) {
  x[["object"]] <- dplyr::filter(x[["object"]], ...)
  tabs <- c("object", "object_link_edge", "edge")
  x[tabs] <- semi_cascade0(x[tabs], tables = tabs)
  x
}

semi_cascade0 <- function (x, ..., tables = c("o", "b", "bXv", "v")) {
  itab <- tables[1L]
  first <- dplyr::filter(x[[itab]], ...)
  x[[itab]] <- last <- first
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- semi_join_1(x[[itab]], 
                                                               last)
  }
  x
}

semi_join_1 <- 
function (x, y, by = NULL, copy = FALSE, ...) 
{
  comm <- base::intersect(names(x), names(y))
  if (length(comm) == 1L) {
    by <- comm
  }
  dplyr::semi_join(x, y, by = by, copy = copy, ...)
}