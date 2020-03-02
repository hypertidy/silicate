#' Given a `PATH`` model decompose to 1-dimensional primitives (or 0-dimensional).
#'
#' @param x input object
#' @param ... arguments passed to methods
#' @importFrom dplyr %>% inner_join mutate
#' @name sc_segment
#' @return data frame of the segments, each occurence of an edge and its order
#' @export
#' @examples
#' sc_segment(SC(minimal_mesh))
sc_segment <- function(x, ...) UseMethod("sc_segment")
#' @name sc_segment
#' @export
sc_segment.default <- function(x, ...) {
  x <- SC(x)
  sc_segment(x, ...)
}
sc_segment.SC <- function(x, ...) {
  ## expand all instances of edges
  segments <- x$object_link_edge %>%
    dplyr::inner_join(x$edge, "edge_")

  ## and badge them as segments
  segments$segment_ <- sc_uid(nrow(segments))
  segments
}
#' @name sc_segment
#' @export
sc_segment.PATH <- function(x, ...) {
  sc_segment_base(x[["path_link_vertex"]])
}


sc_segment_base <- function(path_link_vertex) {
  ## how many vertices on each path?
  frle <- rle(path_link_vertex[["path_"]])
  if (all(frle$lengths == 1L)) {
    return(tibble::tibble(.vx0 = path_link_vertex$vertex_,
                          .vx1 = .data$.vx0,
                          path_ = path_link_vertex$path_,
                          segment_ = sc_uid(nrow(path_link_vertex)),
                          edge_ = sc_uid(length(unique(.data$.vx0)))[factor(.data$.vx0)]))
  }
  ## push into segments
  segtab <- path_to_segment(path_link_vertex[["vertex_"]])
  if (length(frle$values) > 1L) {
    ## this fails if there's only one path in the whole set
    ## fixes https://github.com/hypertidy/silicate/issues/40
    segtab <- segtab[-utils::head(cumsum(frle$lengths), -1L), ]
  }
  segtab[["path_"]] <- rep(frle$values, frle$lengths - 1)
  segtab[["segment_"]] <- sc_uid(nrow(segtab))
  edge <- paste(pmin(segtab[[".vx0"]], segtab[[".vx1"]]), pmax(segtab[[".vx0"]], segtab[[".vx1"]]), sep = "_")
  f <- factor(edge)
  segtab[["edge_"]] <- sc_uid(nlevels(f))[f]
  segtab
}
