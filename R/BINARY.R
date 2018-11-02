#' Binary form
#'
#' Build a purely edge-based form from path-based data.
#'
#' Binary is a two-table form of `object` and `vertex`. On object the `vertex_` is a list column,
#' with `.vx0` and `.vx1`. These are the row numbers of `vertex`.
#' @param x object (sf, sp, anything undertood by silicate/gibble)
#' @param ... ignored
#'
#' @return binary form
#' @noRd
#'
#' @examples
#' plot(BINARY(minimal_mesh))
#' #library(trip)
#' #plot(BINARY(walrus818))
#'
#' #b <- BINARY(rnaturalearth::ne_countries(returnclass = "sp"))
#' #plot(b)
#' # we can subset object trivially, with no need to relabel
#' # because we aren't changing the vertex pool
#' # b$object <- dplyr::filter(b$object, nchar(sovereignt) > 13)
#' # plot(b)
#' @importFrom dplyr arrange
BINARY <- function(x, ...) {

  ## get coordinates
  ## map instances (unique v, expanded v-index)
  ## convert v-index to segments
  ## nest segments with objects

  ## rely on integer vertex-row index (objects can be subset, but vertices need remapping)
  coord0 <- sc_coord(x)
  udata <- unjoin::unjoin(coord0, .data$x_, .data$y_, key_col = "vertex_")

  udata[["vertex_"]]$row <- seq_len(nrow(udata[["vertex_"]]))
  ## anything path-based has a gibble (sp, sf, trip at least)
  ## which is just a row-per path with nrow, and object, subobject, path classifiers
  gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
  instances <- udata$data %>% dplyr::mutate(path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                                            object = rep(gmap$object, gmap$nrow),
                                             coord = row_number())

  ## cx0 and cx1 are the segment vertices, they map the coordinate instances, not the vertices
  segs <- instances %>% dplyr::select(.data$path, .data$coord, .data$object)  %>%
    dplyr::mutate(.cx0 = .data$coord,   ## specify in segment terms
                  .cx1 = .data$coord + 1L) %>%
    dplyr::group_by(.data$path) %>% dplyr::slice(-n()) %>% dplyr::ungroup() %>%
    dplyr::transmute(.data$.cx0, .data$.cx1, .data$path, .data$object)

  object <- sc_object(x)

  if (nrow(segs) < 1) {
 #   stop(sprintf("no segments/edges found in object of class %s", class(x)))
    instances[".vx0"] <- instances["vertex_"]
    object$topology_ <- split(instances[c(".vx0")], instances$object)

  } else {
  segs[[".vx0"]] <- instances$vertex_[match(segs$.cx0, instances$coord)]
  segs[[".vx1"]] <- instances$vertex_[match(segs$.cx1, instances$coord)]
  ## but udata$.idx0 has the vertices, with .idx0 as the mapping
  object$topology_ <- split(segs[c(".vx0", ".vx1")], segs$object)

}
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())

  structure(list(object = object, vertex = udata$vertex_ %>%
                   dplyr::arrange(.data$vertex_) %>% dplyr::select(.data$x_, .data$y_),
                 meta = meta),
            class = c("BINARY", "sc"))
}
# sc_path.BINARY <- function(x, ...) {
#   ## needs path winding
# }
# sc_arc.BINARY <- function(x, ...) {
#   ## needs path winding
# }
sc_segment.BINARY <- function(x, ...) {
  unnest(x$object["edge_"])
}
# sc_edge.BINARY <- function(x, ...) {
#   sc_segment(x)  ## but made unique by sort c(.vx0, .vx1)
# }

sc_object.BINARY <- function(x, ...) {
  o <- x[["object"]]
  o$edge_ <- NULL
  o
}
sc_vertex.BINARY <- function(x, ...) {
  x[["vertex"]]
}
#' @importFrom tidyr unnest
sc_coord.BINARY <- function(x, ...) {
  unn <- tidyr::unnest(x$object)
  ## segment pairs, only .vx1 final one missing from .vx0
  x$vertex[c(unn$.vx0, tail(unn$.vx1, 1)), ]
}
#' Plot BINARY
#'
#' Plot a BINARY model, segments coloured by object.
#'
#' @param x BINARY object
#' @param ... arguments  passed to `graphics::segments` (col is ignored)
#' @noRd
#' @importFrom graphics segments
plot.BINARY <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  unn <- tidyr::unnest(x$object)
  s1 <- x$vertex[unn[[".vx0"]], ]
  s2 <- x$vertex[unn[[".vx1"]], ]
  ## properties are organized by object
  col <- sc_colour_values(rep(seq_len(nrow(x$object)), purrr::map_int(x$object$edge_, nrow)),
                          viridis = TRUE)
  graphics::segments(s1$x_, s1$y_, s2$x_, s2$y_, col = col, ...)
}
