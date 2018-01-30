sc_colours <- function(x, ...) {
  # https://stackoverflow.com/a/33144808/355270
  cl <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  sample(cl, x, replace = x > length(cl))
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
  
#' ARC model
#'
#' Arcs are paths within the line segment graph between nodes. 
#' 
#' Nodes are the vertices where three or more arcs meet. 
#' An arc can exist without including any nodes, a path that has no neighbouring relationship with another path. 
#' 
#' This is _not_ the same terminology as used by other systems. The `arc_link_vertex` mapping is inherently ordered, 
#' but we don't consider order of arcs. Duplicated arcs (i.e. complementary turns around neighbouring polygons) are 
#' not kept. The `object_link_arc` mapping records which arc belongs to the objects, so can be reconstructed within 
#' objects by tracing `arc_link_vertex` start and end point identity.
#' @inheritParams SC
#' 
#' @return ARC model
#' @export
#'
#' @examples
#' a <- ARC(minimal_mesh)
#' sc_arc(a)
#' sc_arc(minimal_mesh)
ARC <- function(x, ...) {
  UseMethod("ARC")
}
#' @name ARC
#' @export
ARC.default <- function(x, ...) {
  ARC(PATH(x), ...)
}
#' @name ARC
#' @export
ARC.PATH <- function(x, ...) {
  o <- sc_object(x)
  arc_map <- sc_arc_PATH(x)
  arc_map <- unique_arcs(arc_map)
  oXa <- arc_map %>% dplyr::distinct(.data$object_, .data$arc_)
  aXv  <-  arc_map %>% dplyr::select(.data$arc_, .data$vertex_)
  v <- sc_vertex(x)
  lst <- list(object = o, 
              object_link_arc = oXa, 
          #    arc = arc, 
              arc_link_vertex = aXv, 
              vertex = v
  )
  structure(lst, class = c("ARC", "sc"))
}
PATH.ARC <- function(x, ...) {
  o <- sc_object(x)
  paths <- dplyr::inner_join(o[1, "object_"], x$object_link_arc) %>% 
    dplyr::inner_join(x$arc_link_vertex)
}
unique_arcs <- function(x, ...) {
  dat <- split(x, x$arc_)
  arc_id <- dat %>% 
    purrr::map(function(.x) paste(first_sort(.x$vertex_), collapse = ""))
  bind_rows(dat[!duplicated(arc_id)])
}

first_sort <- function(x) {
  if (x[1L] > x[length(x)]) {
    rev(x) 
  } else {
    x
  }
}