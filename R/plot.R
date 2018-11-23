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
#'
#' @name plot.SC
#' @param add if `TRUE` add to current plot
#' @export
#' @importFrom graphics plot
#' @importFrom dplyr inner_join anti_join group_by summarize tally filter
plot.SC <- function(x, ..., add = FALSE ) {
  plot(SC0(x), add = add, ...)
}
#' @export
plot.SC0 <- function(x, ... , add = FALSE) {
  args <- list(...)
  if (!"col" %in% names(args)) {
    edge_per_object <- unlist(lapply(x$object$topology_, nrow))
    col <-  rep(sc_colour_values(seq_len(nrow(x$object)), viridis = TRUE), edge_per_object)
    args$col <- col
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
  gg <- x$path %>% dplyr::group_by(.data$object) %>% dplyr::summarize(nn = sum(.data$ncoords_))
  #objcols <- rep(sc_colours(dim(x$object)[1L]), gg$nn)
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

#' @export
plot.PATH0 <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  paths <- split(x$path_link_vertex, x$path_link_vertex$path_)[unique(x$path_link_vertex$path_)]
  #cols <- sc_colours(length(obj))
  gg <- x$path %>% dplyr::group_by(.data$object) %>% dplyr::summarize(nn = sum(.data$ncoords_))
  #objcols <- rep(sc_colours(dim(x$object)[1L]), gg$nn)
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
plot.ARC <- function(x, ..., lwd = 2L) {

  plot(x$vertex[c("x_", "y_")], pch = "")
  a1 <- split(x$arc_link_vertex, x$arc_link_vertex$arc_)
  col <- setNames(sc_colours(length(a1)), names(a1))
  a1 %>% purrr::iwalk(~lines(dplyr::inner_join(.x, x$vertex, "vertex_") %>% dplyr::select(x_, y_), col = col[.y], lwd = lwd))
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
    asub <- dplyr::filter(triangle, .data$object_ == x$object$object_[i]) %>%
      dplyr::transmute(.data$.vx0, .data$.vx1, .data$.vx2, fill = fill_type) %>%
      t() %>%
      as.vector()
    asub <-   tibble::tibble(vertex_ = asub)
    asub <- head(asub, -1L)
    graphics::polypath(dplyr::left_join(asub,x$vertex,  "vertex_") %>% dplyr::select(.data$x_, .data$y_),
                       col = cols[i], ...)

  }
}



oldplot.SC <- function(x, add = FALSE, ..., vars = NULL, use_edge_colour = FALSE) {
  if (!"color_" %in% names(x$object)) {
    x$object$color_ <- sc_colour_values(x$object$object_, viridis = TRUE)
  } else {
    if (use_edge_colour) warning("'color_' property on object table ignored, set 'use_edge_colour = FALSE' to use them")
  }
  v <- sc_vertex(x)
  if (!is.null(vars)) {
    vars <- c(vars, "vertex_")
    v <- dplyr::select(v, vars) %>%
      setNames(c("x_", "y_", "vertex_"))
  }
  e <- sc_edge(x)
  x0 <- e %>% dplyr::inner_join(v, c(".vx0" = "vertex_"))
  x1 <- e %>% dplyr::inner_join(v, c(".vx1" = "vertex_"))
  # browser()
  if (identical(x0, x1)) warning("all edges are degenerate (i.e. a vertex related to itself)")
  if (!use_edge_colour) {
    #col <- colourvalues::colour_values(x$object_link_edge$object_[match(x$edge$edge_, x$object_link_edge$edge_)])
    col <-  x$object$color_[match(x$object_link_edge$object_, x$object$object_)[match(x0$edge_, x$object_link_edge$edge_)]]
  } else {
    ## colours is object, UNLESS the edge is repeated
    uedge <- x$object_link_edge
    twoedge <- uedge %>% group_by(.data$edge_) %>% dplyr::tally() %>% dplyr::filter(n > 1)
    twoedge <- twoedge %>% inner_join(uedge, "edge_") %>% group_by(.data$edge_) %>% summarize(id = paste(.data$object_, collapse = "-"))
    uedge <- anti_join(uedge, twoedge, "edge_")
    keepedges <- c(uedge$edge_, twoedge$edge_)
    props <- c(uedge$object_, twoedge$id)
    # browser()
    col <- sc_colour_values(props, viridis = TRUE)[match(x0$edge_, keepedges)]
  }

  if (!add) graphics::plot(v$x_, v$y_, pch = ".")
  graphics::segments(x0$x_, x0$y_, x1$x_, x1$y_, ..., col = col)
}
