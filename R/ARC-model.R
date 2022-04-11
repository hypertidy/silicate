

#' ARC model
#'
#' Arcs are unique paths that connect nodes. In a polygon layer with shared boundaries, the
#' arcs are the linear features that have no branches.
#'
#' Nodes are the vertices where three or more arcs meet. An arc can exist without including
#' any nodes, a path that has no neighbouring relationship with another path.
#'
#' This is _not_ the same terminology as used by other systems, such as "arc-node". The
#' `arc_link_vertex` mapping is inherently ordered, but we don't consider order of arcs.
#' Duplicated arcs (i.e. complementary turns around neighbouring polygons) are not kept.
#' The `object_link_arc` mapping records which arc belongs to the objects, so feature polygons
#' can in theory be reconstructed within objects by tracing `arc_link_vertex` start and end point
#' identity.
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
ARC.SC <- function(x, ...) {
  stop("ARC not yet implemented for SC")
}
#' @name ARC
#' @export
ARC.PATH <- function(x, ...) {
  if (!all(grepl("polygon", x$path$type, ignore.case = TRUE))) {
    warning(paste("ARC is not well-defined unless used on polygon layers",
                  "please use with caution", sep = "\n"))
  }
  o <- sc_object(x)
  arc <- sc_arc_PATH(x)
  arc_map <- unique_arcs(arc)

  arc$arc0 <- arc$arc_
  arc$arc_ <- arc_map$arc[match(arc$arc0, arc_map$arc0)]

  oXa <- dplyr::distinct(arc, .data$object_, .data$arc_)
##  arc <- dplyr::distinct(arc, .data$arc_, .data$vertex_)
  arc <- arc[c("arc_", "vertex_")]
  aXv <- do.call(rbind, split(arc, arc$arc_)[unique(arc_map$arc)])
  aXv$object_ <- aXv$arc0 <-  NULL
  v <- sc_vertex(x)

  meta <- tibble::tibble(proj = get_projection(x), ctime = format(Sys.time(), tz = "UTC"))

  lst <- list(object = o,
              object_link_arc = oXa,
          #    arc = arc,
              arc_link_vertex = aXv,
              vertex = v, meta = meta
  )
  structure(lst, class = c("ARC", "sc"), join_ramp = c("object", "object_link_arc", "arc_link_vertex", "vertex"))
}
# PATH.ARC <- function(x, ...) {
#   o <- sc_object(x)
#   paths <- dplyr::inner_join(o[1, "object_"], x$object_link_arc) %>%
#     dplyr::inner_join(x$arc_link_vertex)
#   o <- o[match(paths$object_, o$object_), ]
#   o$object_ <- paths$arc_
#
# }
unique_arcs <- function(x, ...) {
  dat <- split(x, x$arc_)[unique(x$arc_)]
  arc_id <- dat %>%
    purrr::map(function(.x) paste(first_sort(.x$vertex_), collapse = ""))
  tibble::tibble(arc0 = names(dat), arc = names(dat)[ match(unlist(arc_id), unlist(arc_id))])
}

first_sort <- function(x) {
  if (x[1L] > x[length(x)]) {
    rev(x)
  } else {
    x
  }
}
