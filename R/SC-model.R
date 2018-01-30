

#' The universal model
#' 
#' The universal model `SC` is coordinates and binary relations between
#' pairs of coordinates. This is purely an edge (or segment) model, with all 
#' higher level structures recorded as groupings of edges. 
#' @param x input model
#' @param ... arguments passed to methods
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
#' @name SC
#' @param vars variables to plot
#' @export
#' @importFrom graphics plot
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
  col <- grDevices::rainbow(nlevels(idx))[idx]
  graphics::plot(v$x_, v$y_, pch = ".")
  graphics::segments(x0$x_, x0$y_, x1$x_, x1$y_, ..., col = col)
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