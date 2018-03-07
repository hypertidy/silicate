sc_colours <- function(x, ...) {
  # https://stackoverflow.com/a/33144808/355270
  #cl <- grDevices::colors()[-1L]

  cl <- hsv(runif(x), 1, runif(x)) #viridis::viridis(x)
  sample(cl, x, replace = x > length(cl))
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
 if (identical(x0, x1)) warning("all edges are degenerate (i.e. a vertex related to itself)")
  idx <- factor(x$object_link_edge$object_)[seq(1, nrow(e))]
  col <- grDevices::rainbow(nlevels(idx))[idx]
  graphics::plot(v$x_, v$y_, pch = ".")
  graphics::segments(x0$x_, x0$y_, x1$x_, x1$y_, ..., col = col)
}


#' @export
plot.PATH <- function(x, ...) {
  plot(x$vertex[c("x_", "y_")], type = "n")
  paths <- split(x$path_link_vertex, x$path_link_vertex$path_)[unique(x$path_link_vertex$path_)]
  #cols <- sc_colours(length(obj))
  gg <- x$path %>% dplyr::group_by(object) %>% dplyr::summarize(nn = sum(ncoords_))
  objcols <- rep(sc_colours(dim(x$object)[1L]), gg$nn)
  if (all(x$path$ncoords_ == 1L)) {
    warning("all paths are degenerate (i.e. they are points)")
    toplot <- dplyr::inner_join(x$path_link_vertex, x$vertex, "vertex_")[c("x_", "y_")]
    points(toplot, col = objcols)
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
#' @param x  
#' @param ... 
#' @param lwd 
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
  for (i in seq_len(nrow(x$object))) { 
    triangle <- dplyr::inner_join(x$triangle, x$object_link_triangle)
    asub <- dplyr::filter(triangle, .data$object_ == x$object$object_[i]) %>% 
      dplyr::transmute(.data$.vertex0, .data$.vertex1, .data$.vertex2, fill = NA_character_) %>% 
      t() %>% 
      as.vector() 
    asub <-   tibble::tibble(vertex_ = asub)
    asub <- head(asub, -1L)
    graphics::polypath(dplyr::left_join(asub,x$vertex,  "vertex_") %>% dplyr::select(.data$x_, .data$y_), 
                       col = cols[i], ...)
    
  }
}

