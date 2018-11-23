path_paste <- function(x, paster = function(...) paste(..., sep = "-")) {
  ## we are looking for  any of these three
  do.call(paster, x[intersect(names(x), c("object", "subobject", "path"))])
}


#' Pure edge model, structural form
#'
#' `SC0` is the simplest and most general of all silicate models. Composed of
#' an `object` and and `vertex` table linked by nested vertex-index pairs.
#'
#'
#' @param x an object understood by silicate
#' @param ... reserved for methods
#'
#' @return SC0
#' @export
#'
#' @examples
#' SC0(minimal_mesh)
#' SC0(minimal_mesh)
SC0 <- function(x, ...) {
  UseMethod("SC0")
}
#' @name SC0
#' @importFrom dplyr mutate slice group_by ungroup select
#' @importFrom gibble gibble
#' @export
SC0.default <- function(x, ...) {
  coord0 <- sc_coord(x)
  udata <- unjoin::unjoin(coord0, .data$x_, .data$y_, key_col = "vertex_")
  udata[["vertex_"]]$row <- seq_len(nrow(udata[["vertex_"]]))
  gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
  instances <- udata$data %>% dplyr::mutate(path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                                            object = rep(gmap$object, gmap$nrow),
                                            coord = row_number())

  object <- sc_object(x)

  if (length(unique(instances$path)) == nrow(instances)) {
    ## we are only points
    #   stop(sprintf("no segments/edges found in object of class %s", class(x)))
    instances[".vx0"] <- instances["vertex_"]
    object$topology_ <- split(instances[c(".vx0")], instances$object)

  } else {
    ## cx0 and cx1 are the segment vertices, they map the coordinate instances, not the vertices
    segs <- instances %>% dplyr::select(.data$path, .data$coord, .data$object)  %>%
      dplyr::mutate(.cx0 = .data$coord,   ## specify in segment terms
                    .cx1 = .data$coord + 1L) %>%
      dplyr::group_by(.data$path) %>% dplyr::slice(-n()) %>% dplyr::ungroup() %>%
      dplyr::transmute(.data$.cx0, .data$.cx1, .data$path, .data$object)

    segs[[".vx0"]] <- instances$vertex_[match(segs$.cx0, instances$coord)]
    segs[[".vx1"]] <- instances$vertex_[match(segs$.cx1, instances$coord)]
    ## but udata$.idx0 has the vertices, with .idx0 as the mapping
    object$topology_ <- split(segs[c(".vx0", ".vx1")], segs$object)

  }
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())

  structure(list(object = object, vertex = udata$vertex_ %>%
                   dplyr::arrange(.data$vertex_) %>% dplyr::select(.data$x_, .data$y_),
                 meta = meta),
            class = c("SC0", "sc"))
}
#' @export
SC0.SC0 <- function(x, ...) {
  x
}
#' @export
SC0.SC <- function(x, ...) {
  object <- sc_object(x)
  oXe <- x$object_link_edge %>%
    dplyr::inner_join(sc_edge(x), "edge_")
  .vx  <- cbind(match(oXe[[".vx0"]], x$vertex[["vertex_"]]),
                match(oXe[[".vx1"]], x$vertex[["vertex_"]]))
  ## swap order if not native instance
  swap <- !oXe[["native_"]]
  ## doing my head in atm ... could be better
  if (length(swap) > 0) .vx  <- cbind(.vx0 = .vx[cbind(1:nrow(.vx), swap + 1)],
                                      .vx1 = .vx[cbind(1:nrow(.vx), (!swap) + 1)])
  object[["topology_"]] <- split(tibble::as_tibble(.vx), as.integer(factor(oXe$object_,unique(oXe$object_))))
  structure(list(object = object, vertex = sc_vertex(x) %>% dplyr::mutate(vertex_ = NULL)), class = c("SC0", "sc"))
}
sc_vertex.SC0 <- function(x, ...) {
  x[["vertex"]]
}

#' Plot silicate
#'
#' Plot a SC0 model, primitives coloured by object.
#'
#' @param x SC0 object
#' @param ... arguments  passed to `graphics::segments` (col is ignored)
#' @export
#' @importFrom graphics segments
plot.SC0 <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  unn <- tidyr::unnest(x$object)
  topology_dim <- dim(x$object$topology_[[1]])[2L]
  ## properties are organized by object
  col <- sc_colour_values(rep(seq_len(nrow(x$object)), purrr::map_int(x$object$topology_, nrow)),
                                     viridis = TRUE)
  if (topology_dim == 2) {
    s1 <- x$vertex[unn[[".vx0"]], ]
    s2 <- x$vertex[unn[[".vx1"]], ]
    graphics::segments(s1$x_, s1$y_, s2$x_, s2$y_, col = col, ...)

  } else {
    s1 <- x$vertex[unn[[".vx0"]], ]
    graphics::points(s1$x_, s1$y_, col = col, ...)
  }
invisible(NULL)
}
