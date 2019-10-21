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
#' start and end coordinates by joining on the appropriate edge vertex label, `.vx0`
#' or `.vx1`. Currently this returns the ordered segments, along with their unique (unordered) `edge_`, as
#' well as unique `segment`, a `object_` labels.
#' @param x input object
#' @param ... arguments for methods
#' @name sc_edge
#' @return data frame of edge identity, or start/end coordinates
#' @aliases sc_start sc_end
#' @export
sc_edge <- function(x, ...) {
  UseMethod("sc_edge")
}
#' @name sc_edge
#' @export
sc_edge.default <- function(x, ...) {
  x <- SC0(x, ...)
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
  # if (is.null(x$object[["object_"]])) {
  #   x$object$object_ <- seq_len(dim(x$object)[1])
  # }
 out <- tidyr::unnest(x$object[c( "topology_")], cols = c(.data$topology_))
 out$edge_ <- as.integer(as.factor(paste(pmin(out$.vx0, out$.vx1), pmax(out$.vx0, out$.vx1))))
 out <- out %>% dplyr::distinct(.data$edge_, .keep_all = TRUE)
 out$edge_ <- NULL
 out
}

#' @name sc_edge
#' @export
sc_edge.PATH <- function(x, ...) {
  sc_segment(x, ...) %>% dplyr::distinct(.data$edge_, .keep_all = TRUE) %>%
    dplyr::select(.data$.vx0, .data$.vx1, .data$edge_)
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
#' @importFrom utils head
sc_start.PATH <- function(x, ...) {
  pth_rle <- sc_path(x)$ncoords_
  start_idx <- c(1L)
  if (length(pth_rle) > 1L) {
    start_idx <- cumsum(c(start_idx, utils::head(pth_rle, -1)))
  }
  (x$path_link_vertex[start_idx, ] %>% dplyr::inner_join(x$vertex, "vertex_"))[c("x_", "y_", "path_","vertex_")]
}
#' @name sc_edge
#' @export
sc_end.PATH <- function(x, ...) {
  pth_rle <- sc_path(x)$ncoords_
  end_idx <- pth_rle[1L]
  if (length(pth_rle) > 1L) {
    end_idx <- cumsum(c(end_idx, tail(pth_rle, -1)))
  }
  (x$path_link_vertex[end_idx, ] %>% dplyr::inner_join(x$vertex, "vertex_"))[c("x_", "y_", "path_","vertex_")]
}
#' @name sc_edge
#' @export
sc_start.PATH0 <- function(x, ...) {
  ## FIXME:: avoid conversion to PATH
  sc_start(PATH(x), ...)
}
#' @name sc_edge
#' @export
sc_end.PATH0 <- function(x, ...) {
  ## FIXME:: avoid conversion to PATH
  sc_end(PATH(x), ...)
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
sc_end.ARC <- function(x, ...) {
  sc_end(SC(x), ...)
}
#' @name sc_edge
#' @export
sc_end.TRI <- function(x, ...) {
  sc_end(SC(x), ...)
}
