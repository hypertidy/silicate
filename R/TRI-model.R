#' TRI model, triangulations
#'
#' TRI creates a constrained triangulation using 'ear-cutting', or 'ear-clipping' of
#' polygons. The model is a 'relational' form in that the underlying tables are 
#' linked implicitly by unique identifiers.  
#' Ear-cutting is inherently path-based, so this model is only available for
#' path-based structures, like simple features, [PATH()], [PATH0()] and [ARC()].
#' @param x object understood by silicate (sf, sp, a silicate model, etc.)
#' @param ... current unused
#' @param add logical create  new plot (default), or add to existing
#' @return TRI model with tables 'object', 'triangle', 'vertex'
#' @export
#' @examples
#' tri <- TRI(minimal_mesh)
#' plot(tri)
TRI <- function(x, ...) {
  UseMethod("TRI")
}
#' @export
TRI.default <- function(x, ...) {
  ## TRI is earcut, so must be PATH based
  TRI(PATH(x), ...)
}
#' @export
TRI.TRI <- function(x, ...) {
  x
}
TRI.SC <- function(x, ...) {
  stop("constrained triangulation not supported, use anglr::DEL or reconstruct as PATH")
}
#' @export
TRI.TRI0 <- function(x, ...){
  topol <- dplyr::bind_rows(x$object$topology_, .id = "object_")
  x$object$topology_ <- NULL
  x$object$object_ <- sc_uid(x$object)
  topol$object_ <- x$object$object_[as.integer(topol$object_)]
  v <- sc_vertex(x)
  v$vertex_ <- sc_uid(v)
  topol$.vx0 <- v$vertex_[topol$.vx0]
  topol$.vx1 <- v$vertex_[topol$.vx1]
  topol$.vx2 <- v$vertex_[topol$.vx2]
  meta <- x$meta[1,]
  meta$ctime <- Sys.time()
  structure(list(object = x$object, triangle = topol,
                 vertex = v, meta = rbind(meta, x$meta)), class = c("TRI", "sc"))


}
#' @export
TRI.PATH0 <- function(x, ...) {
  TRI(PATH(x), ...)
}
#' @export
TRI.PATH <- function(x, ...) {
  vertex <- sc_vertex(x)
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  if (all(x$path$ncoords_ < 2)) stop("TRI for PATH cannot include degenerate paths, see '.$path$ncoords_'")
  if (any(x$path$ncoords_ < 3)) {
    warning("filtering out paths with fewer than 3 coordinates before attempting triangulation by ear clipping")
    x$path <- x$path %>% dplyr::filter(.data$ncoords_ > 2)
  }
  ## pretty sure I'll live to regret this ...
  ## (but the right alternative is a smart DEL visibility classifier )
  ## if we get lines, just pretend they all independently POLYGON
  if (!"subobject" %in% names(x$path)) {
    warning("assuming that all paths are independent (i.e. all islands, no holes)")
    ##x$path$subobject <- 1
    x$path <- x$path %>% dplyr::group_by(.data$object_) %>%
      dplyr::mutate(subobject = row_number(),
                    #subobject = .data$subobject_,
                    object = .data$object_) %>%
      dplyr::ungroup()
  }
  tri <- triangulate_0(x)
  tri$visible <- TRUE
  tri$path_ <- NULL

  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())

  structure(list(object = obj, #object_link_triangle = oXt,
                 triangle = tri,
                 vertex = sc_vertex(x),
                 meta = meta), class = c("TRI", "sc"))
}
#' @name sc_object
#' @export
sc_object.TRI <- function(x, ...) {
  x[["object"]]
}







