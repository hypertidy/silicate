#' Silicate colours
#'
#' Simple set of colours for discrete palette.
#'
#' @param x number of colours to generate
#' @param ... currently ignored
#' @param viridis use viridis, TRUE or FALSE
#'
#' @return colours
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

sc_colour_values <- function(x, ..., viridis = FALSE) {
  if (!is.numeric(x)) x <- as.integer(factor(x))
  cols <- sc_colours(256, viridis = viridis)
  if (length(x) == 1L) return(sample(cols, 1))  ## a fun easter egg
  scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
  cols[scl(x) * (length(cols)-1) + 1]
}
#' Plot silicate
#'
#' Basic edge plot, all the standard base graphics facilities for line segments are available.
#'
#' The 'col' argument is passed directly to `segments` if present, in the usual one-to-one or
#' recycling way. The `...` args are passed to `segments`.  If `color_` present on the object table
#' it is mapped to the edges of each object.
#'
#' `asp` and so on are not able to be passed to the initial plot setup.
#' @name plot.SC
#' @param x SC object
#' @param ... arguments passed to `graphics::segments`
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
  if ("color_" %in% names(x$object)) {
      edge_per_object <- unlist(lapply(x$object$topology_, nrow))
      args$col <- rep(x$object$color_, edge_per_object)

  }
  v <- sc_vertex(x) %>% add_rownum("vertex_")
  e <- sc_edge(x)
  x0 <- e %>% dplyr::inner_join(v, c(".vx0" = "vertex_"))
  x1 <- e %>% dplyr::inner_join(v, c(".vx1" = "vertex_"))
  if (!add) graphics::plot(v$x_, v$y_, pch = ".")
  args$x0 <- x0$x_
  args$x1 <- x1$x_
  args$y0 <- x0$y_
  args$y1 <- x1$y_
  do.call(graphics::segments, args)
#  graphics::segments(x0$x_, x0$y_, x1$x_, x1$y_, col = col, ...)
invisible(args)
}

#' @export
plot.PATH <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  paths <- split(x$path_link_vertex, x$path_link_vertex$path_)[unique(x$path_link_vertex$path_)]
  #cols <- sc_colours(length(obj))
  gg <- x$path %>% dplyr::group_by(.data$object_) %>% dplyr::summarize(nn = sum(.data$ncoords_))

  pathcols <- sc_colours(dim(x$object)[1L])[factor(x$path$object_)]
  if (all(x$path$ncoords_ == 1L)) {
    warning("all paths are degenerate (i.e. they are points)")
    toplot <- dplyr::inner_join(x$path_link_vertex, x$vertex, "vertex_")[c("x_", "y_")]
    graphics::points(toplot, col = rep(sc_colours(dim(x$object_)[1L]), gg$nn))
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
plot.PATH0 <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  paths <- split(x$path_link_vertex, x$path_link_vertex$path_)[unique(x$path_link_vertex$path_)]
  #cols <- sc_colours(length(obj))
  gg <- x$path %>% dplyr::group_by(.data$object) %>% dplyr::summarize(nn = sum(.data$ncoords_))
  objcols <- sc_colours(dim(x$object)[1L])
  if (all(x$path$ncoords_ == 1L)) {
    warning("all paths are degenerate (i.e. they are points)")
    toplot <- dplyr::inner_join(x$path_link_vertex, x$vertex, "vertex_")[c("x_", "y_")]
    graphics::points(toplot, col = objcols)
    return(invisible(NULL))
  }
  junk <- lapply(seq_along(paths), function(a) {
    toplot <- dplyr::inner_join(paths[[a]], x$vertex, "vertex_")[c("x_", "y_")]
    if (dim(toplot)[1L] > 1L) {
      graphics::lines(toplot, col = objcols[a])
    } else {
      graphics::points(toplot, col = objcols[a])
    }
  })
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
plot.ARC <- function(x, ..., lwd = 2L) {

  plot(x$vertex[c("x_", "y_")], pch = "")
  a0 <- dplyr::inner_join(x$arc_link_vertex, x$vertex, "vertex_")
  a0$vertex_ <- NULL
  a1 <-   split(a0, a0$arc_)
#  a1 <- split(x$arc_link_vertex, x$arc_link_vertex$arc_)
  col <- rep(sc_colours(length(a1)), purrr::map_int(a1, nrow))
  p2s <- function(x) cbind(.vx0 = utils::head(x, -1L),
                          .vx1 = utils::tail(x, -1))
segs <-   do.call(rbind, purrr::map(a1, ~p2s(as.matrix(.x[c("x_", "y_")]))))

  graphics::segments(segs[,1], segs[,2], segs[,3], segs[,4], col = col)
}

#' @name TRI
#' @export
plot.TRI <- function(x, ..., add = FALSE) {

  if (!add) plot(x$vertex[c("x_", "y_")], type = "n")
  cols <- sc_colours(nrow(sc_object(x)))
  fill_type <- if (getOption("silicate.uid.type") == "integer") NA_integer_ else NA_character_
  for (i in seq_len(nrow(x$object))) {
   # triangle <- dplyr::inner_join(x$triangle, x$object_link_triangle, "triangle_")
    triangle <- x$triangle
    if ("visible_" %in% names(triangle)) triangle <- dplyr::filter(triangle, .data$visible_)
    asub <- dplyr::filter(triangle, .data$object_ == x$object$object_[i]) %>%
      dplyr::transmute(.data$.vx0, .data$.vx1, .data$.vx2, fill = fill_type) %>%
      t() %>%
      as.vector()
    asub <-   tibble::tibble(vertex_ = asub)
    asub <- utils::head(asub, -1L)
    graphics::polypath(dplyr::left_join(asub,x$vertex,  "vertex_") %>% dplyr::select(.data$x_, .data$y_),
                       col = cols[i], ...)

  }
}

