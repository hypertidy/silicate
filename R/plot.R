#' Silicate colours
#'
#' Simple set of colours for discrete palette.
#'
#'
#' @param x number of colours to generate
#' @param ... currently ignored
#' @param viridis use viridis, TRUE or FALSE
#'
#' @return vector of colours
#' @export
#'
#' @examples
#' sc_colours(10)
sc_colours <- function(x = 16, ..., viridis = FALSE) {
  # https://stackoverflow.com/a/33144808/355270
  #cl <- grDevices::colors()[-1L]
 if (viridis) {
   return(grDevices::colorRampPalette(viridis_cols)(x))
 }
  cl <- grDevices::hsv(stats::runif(x), 1, stats::runif(x))
  sample(cl, x, replace = x > length(cl))
}


#' Plot silicate
#'
#' Basic edge plot, all the standard base graphics facilities for line segments are available.
#'
#' The 'col' argument is passed directly to [segments()]  or [polypath()] as needed, in the usual
#' one-to-one or recycling way.
#'
#' Graphical parameters are not able to be passed to the initial plot setup, but a plot
#' can be set up and then added to with this method.
#' @name plot.SC
#' @param x sc object
#' @param ... arguments passed to lower level plotting functions
#' @param add if `TRUE` add to current plot
#' @export
#' @rdname plot.SC
#' @importFrom graphics plot
#' @importFrom dplyr inner_join anti_join group_by summarize tally filter
plot.SC <- function(x, ..., add = FALSE ) {
  plot(SC0(x), add = add, ...)
}
#' @export
#' @rdname plot.SC
plot.SC0 <- function(x, ... , add = FALSE) {
  args <- list(...)
  ## no colours for free, you get what you get
  edge_per_object <- unlist(lapply(x$object$topology_, nrow))

  if ("color_" %in% names(x$object)) {
    args$col <- rep(x$object$color_, edge_per_object)
  } else {
    if ("col" %in% names(args)) {
      args$col <- rep(args$col, sum(edge_per_object))
    }
  }

  v <- sc_vertex(x) %>% add_rownum("vertex_")
  e <- sc_edge(x)
  x0 <- e %>% dplyr::inner_join(v, c(".vx0" = "vertex_"))
  x1 <- e %>% dplyr::inner_join(v, c(".vx1" = "vertex_"))
  if (!add) {
   .setup_plot(v, ...)
  }

  args$x0 <- x0$x_
  args$x1 <- x1$x_
  args$y0 <- x0$y_
  args$y1 <- x1$y_
  do.call(graphics::segments, args)
#  graphics::segments(x0$x_, x0$y_, x1$x_, x1$y_, col = col, ...)
invisible(args)
}

#' @export
plot.PATH <- function(x, ..., add = FALSE) {

  if (!add) {
    v <- sc_vertex(x)
   .setup_plot(v, ...)
  }

  paths <- split(x$path_link_vertex, x$path_link_vertex$path_)[unique(x$path_link_vertex$path_)]
  #cols <- sc_colours(length(obj))
  gg <- x$path %>% dplyr::group_by(.data$object_) %>% dplyr::summarize(nn = sum(.data$ncoords_))

  ##pathcols <- sc_colours(dim(x$object)[1L])[factor(x$path$object_)]
  pathcols <- rep("black", dim(x$object)[1L])
  if (!is.null(list(...)$col)) pathcols <- rep_len(list(...)$col, length.out = dim(x$object)[1L])
  pathcols <- pathcols[factor(x$path$object_)]
  if (all(x$path$ncoords_ == 1L)) {
    warning("all paths are degenerate (i.e. they are points)")
    toplot <- dplyr::inner_join(x$path_link_vertex, x$vertex, "vertex_")[c("x_", "y_")]
    graphics::points(toplot, col = rep(sc_colours(dim(x$object)[1L]), gg$nn))
    return(invisible(NULL))
  }
  junk <- lapply(seq_along(paths), function(a) {
    toplot <- dplyr::inner_join(paths[[a]], x$vertex, "vertex_")[c("x_", "y_")]
    if (dim(toplot)[1L] > 1L) {
      graphics::lines(toplot, col = pathcols[a])
    } else {
      graphics::points(toplot, col = pathcols[a])
    }
  })
  invisible(NULL)
}

#' @export
plot.PATH0 <- function(x, ..., add = FALSE) {
  if (!add) {
    v <- sc_vertex(x)
   .setup_plot(v, ...)
  }

  col <- sc_colours(nrow(x$object))

  pp <-
  function(x, paster = function(...) paste(..., sep = "-")) {
    ## we are looking for  any of these three
    do.call(paster, x[intersect(names(x), c("object_", "subobject", "path_"))])
  }
  for (i in seq_along(col)) {
    paths <- split(x$object$path_[[i]],
                   pp(x$object$path_[[i]]))
    purrr::map(paths, ~{
      lines(x$vertex[.x$vertex_, c("x_", "y_")], col = col[i])
    })

  }
  invisible(NULL)
}
#' @noRd
#'
#' @param x silicate ARC
#' @param ... ignored
#' @param lwd line width
#'
#' @name ARC
#' @export
#' @importFrom graphics segments
plot.ARC <- function(x, ...,  col = NULL, lwd = 2L, add = FALSE) {

  if (!add) {
    v <- sc_vertex(x)

   .setup_plot(v, ...)
  }

  a0 <- dplyr::inner_join(x$arc_link_vertex, x$vertex, "vertex_")
  a0$vertex_ <- NULL
  a1 <-   split(a0, a0$arc_)
#  a1 <- split(x$arc_link_vertex, x$arc_link_vertex$arc_)
  if (is.null(col)) {
    col <- sc_colours(length(a1))
  }
  col <- rep(col, purrr::map_int(a1, nrow) - 1)
  p2s <- function(x) cbind(.vx0 = utils::head(x, -1L),
                          .vx1 = utils::tail(x, -1))
segs <-   do.call(rbind, purrr::map(a1, ~p2s(as.matrix(.x[c("x_", "y_")]))))
  graphics::segments(segs[,1], segs[,2], segs[,3], segs[,4], col = col, lwd = lwd, ...)
}

#' @name TRI
#' @export
plot.TRI <- function(x, ..., add = FALSE) {
  v <- x$vertex
  if (!add) {
    v <- sc_vertex(x)

   .setup_plot(v, ...)
  }

  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)
  tt <- x[["triangle"]]
  if (!is.null(tt[["visible"]]))  tt <- dplyr::filter(tt, .data$visible)

  tt <- match(as.vector(t(as.matrix(tt[c(".vx0", ".vx1", ".vx2")]))), v$vertex_)
  xx <- tibble(x = v$x_[tt], y = v$y_[tt], id = rep(seq_len(length(tt)/3), each = 3),
               col = NA, border = "black")

  args <- list(...)
  if (!is.null(args$col)) {
    xx$col <- rep_len(args$col, length.out = nrow(xx))
    xx$border <- NA
  }
  if (!is.null(args$border)) xx$border <- rep_len(args$border, length.out = nrow(xx))
  grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = xx$border, fill = xx$col),
                     default.units = "native")
  grid::popViewport(3)
}

#' @export
plot.TRI0 <- function(x, ..., add = FALSE) {
  v <- x$vertex
  if (!add) {
    v <- sc_vertex(x)

   .setup_plot(v, ...)
  }
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)
  tt <- t(as.matrix(dplyr::bind_rows(x$object$topology_)[c(".vx0", ".vx1", ".vx2")]))

  xx <- tibble(x = v$x_[tt], y = v$y_[tt], id = rep(seq_len(length(tt)/3), each = 3),
               col = NA, border = "black")
  args <- list(...)
  if (!is.null(args$col)) {
    xx$col <- rep_len(args$col, length.out = nrow(xx))
    xx$border <- NA
  }
  if (!is.null(args$border)) xx$border <- rep_len(args$border, length.out = nrow(xx))
  grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = xx$border, fill = xx$col),
                     default.units = "native")
  grid::popViewport(3)

}

.setup_plot <- function(x, ...) {
  if ("type" %in% names(list(...))) {
    plot(x$x_, x$y_,  ...)
  } else {
    plot(x$x_, x$y_, type = "n", ...)
  }
}
