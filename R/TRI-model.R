
triangulate_0 <- function(x, ...) {
  objlist <- split(x$path, x$path$object_)
  objlist <- objlist[unique(x$path$object_)]
  polygon_count <- nrow(dplyr::distinct(x$path[c("object_", "subobject")]))
  trilist <- vector("list", polygon_count)
  itri <- 0
  for (i in seq_along(objlist)) {
    #obj <- objlist[[i]]
    #browser()
    if (length(unique(objlist[[i]]$subobject)) > 1) {
      subobjlist <- split(objlist[[i]], objlist[[i]]$subobject)
      subobjlist <- subobjlist[unique(objlist[[i]]$subobject)]
    } else {
      subobjlist <- objlist[i]
    }
    for (j in seq_along(subobjlist)) {
      itri <- itri + 1
      verts <- subobjlist[[j]] %>%
        dplyr::select("object_", "path_") %>%
        dplyr::inner_join(x$path[c("path_", "object_")], "path_") %>%
        dplyr::select("path_") %>%
        dplyr::inner_join(x$path_link_vertex, "path_") %>%
        dplyr::inner_join(x$vertex, "vertex_")
      holes <- which(c(0, abs(diff(as.integer(as.factor(verts$path_))))) > 0)
      if (length(holes) < 1) holes <- 0

      trindex <- decido::earcut(cbind(verts[["x_"]], verts[["y_"]]), holes)
      trimat <- matrix(trindex, ncol = 3L, byrow = TRUE)
      trilist[[itri]] <- tibble::tibble(.vx0 = verts$vertex_[trimat[,1L]],
                                        .vx1 = verts$vertex_[trimat[,2L]],
                                        .vx2 = verts$vertex_[trimat[,3L]],
                                        path_ = verts$path_[1L],
                                        object_ = objlist[[i]]$object_[1L])

    }
  }
  dplyr::bind_rows(trilist)
}



#' TRI model, triangulations
#'
#' TRI creates a constrained triangulation of polygons. The model is a
#' 'relational' form in that the underlying tables are linked implicitly by
#' unique identifiers.
#'
#' On the silicate2 branch each polygon is triangulated as a constrained
#' Delaunay triangulation, with every ring edge a constraint and holes
#' found by constraint depth (so ring orientation does not matter), by the
#' 'cdtr' engine, or 'laridae', through `meshcore::tri_mesh()`. With
#' `engine = "earcut"`, or when 'meshcore' and 'cdtr' are not installed,
#' polygons are triangulated by ear-cutting ('decido') as before.
#'
#' Constrained Delaunay triangles are better shaped than ear-cut ones, and
#' can be refined: `max_area` and `min_angle` passed in `...` go to the
#' engine, and the new (Steiner) vertices are added to the vertex table
#' with numeric vertex attributes (such as `z_`) interpolated linearly.
#' Self-intersecting rings are resolved by inserting the crossing points as
#' new vertices. Each polygon is triangulated on its own, as ear-cutting
#' does, so a Steiner vertex on an edge shared by two polygons is not
#' shared between them.
#'
#' Triangulation is inherently path-based, so this model is only available
#' for path-based structures, like simple features, [PATH()], [PATH0()] and
#' [ARC()].
#' @param x object understood by silicate (sf, sp, a silicate model, etc.)
#' @param ... passed to the triangulation engine (for example `max_area`
#'   and `min_angle`); ignored by `engine = "earcut"`
#' @param engine `"auto"` (the default: 'cdtr' when it and 'meshcore' are
#'   installed, otherwise ear-cutting), `"cdtr"`, `"laridae"` or `"earcut"`.
#'   Set a session default with `options(silicate.tri_engine = )`.
#' @param add logical create  new plot (default), or add to existing
#' @return TRI model with tables 'object', 'triangle', 'vertex'
#' @export
#' @examples
#' tri <- TRI(minimal_mesh)
#' plot(tri)
#' if (requireNamespace("meshcore", quietly = TRUE) &&
#'     requireNamespace("cdtr", quietly = TRUE)) {
#'   plot(TRI(minimal_mesh, max_area = 0.01, min_angle = 25))
#' }
TRI <- function(x, ...) {
  UseMethod("TRI")
}
#' @export
TRI.default <- function(x, ...) {
  ## TRI is earcut, so must be PATH based
  TRI(PATH(x), ...)
}
#' @export
TRI.mesh3d <- function(x, ...) {
  TRI(TRI0(x), ...)
}

#' @export
TRI.TRI <- function(x, ...) {
  x
}
#' @name TRI
#' @export
TRI.SC <- function(x, ...) {
  stop("constrained triangulation not supported, use anglr::DEL or reconstruct as PATH")
}
#' @export
TRI.TRI0 <- function(x, ...){
  #browser()
  topol <- dplyr::bind_rows(x$object$topology_, .id = "object_")
  x$object$topology_ <- NULL
  x$object$object_ <- sc_uid(x$object)
  topol$object_ <- x$object$object_[as.integer(factor(topol$object_))]
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
#' @name TRI
#' @export
TRI.sfc_GEOMETRYCOLLECTION <- function(x, ...) {
  TRI(TRI0(x), ...)
}
#' @name TRI
#' @export
TRI.PATH <- function(x, ..., engine = getOption("silicate.tri_engine", "auto")) {
  engine <- tri_engine(engine)
  vertex <- sc_vertex(x)
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  if (all(x$path$ncoords_ < 2)) stop("TRI for PATH cannot include degenerate paths, see '.$path$ncoords_'")
  if (any(x$path$ncoords_ < 3)) {
    warning("filtering out paths with fewer than 3 coordinates before triangulation")
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

                    object = .data$object_) %>%
      dplyr::ungroup()
  }

  if (engine == "earcut") {
    tri <- triangulate_0(x)
    vertex <- sc_vertex(x)
  } else {
    res <- triangulate_engine(x, engine, ...)
    tri <- res$triangle
    vertex <- res$vertex
  }
  tri$visible <- TRUE
  tri$path_ <- NULL

  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())

  structure(list(object = obj, #object_link_triangle = oXt,
                 triangle = tri,
                 vertex = vertex,
                 meta = meta), class = c("TRI", "sc"))
}
#' @name sc_object
#' @export
sc_object.TRI <- function(x, ...) {
  x[["object"]]
}







