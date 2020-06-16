new_TRI0 <- function(vertex, object, index, crs = NA_character_, meta = NULL) {
  meta1 <- tibble::tibble(proj = crs, ctime = Sys.time())
  if (!is.null(meta)) {
    meta <- rbind(meta1, meta)
  }
  object[["topology_"]] <- index
  structure(list(object = object, vertex = vertex,
                 meta = meta), class = c("TRI0", "sc"))
}

#' TRI0 model, structural triangulations
#'
#' TRI0 creates a constrained triangulation using 'ear-cutting', or 'ear-clipping' of
#' polygons. It is a 'structural' form, a denser storage mode than 'relational'
#' as used by [TRI()], we trade some generality for size and speed.
#'
#' TRI0 is suitable for simple conversion to other mesh forms. See
#' the examples for plotting and (in commented code) conversion to
#' rgl's 'mesh3d'.
#'
#' 'Structural' means that the model does not store relational
#' IDs between tables, the vertex indexing is stored as a nested
#' list of data frames in the 'object' table. Unlike [TRI()] we
#' cannot arbitrarily rearrange the order or remove content
#' of the underlying tables, without updating the vertex indexes
#' stored for each object.
#'
#'
#' Ear-cutting is inherently path-based, so this model is only available for
#' path-based structures, like simple features, [PATH()], [PATH0()] and [ARC()].
#'
#' There is limited support for simple features GEOMETRYCOLLECTION, in short if
#' the GC is composed purely of POLYGON type with 4 coordinates each this is
#' assumed to be a collection of triangles and is converted directly without
#' any triangulation performed. GEOMETRYCOLLECTION of any other form is not
#' supported.
#' @param x object understood by silicate (sf, sp, a silicate model, etc.)
#' @param ... currently unused
#' @return TRI0 model with tables 'object', 'vertex'
#' @seealso TRI
#' @export
#' @examples
#' tri <- TRI0(minimal_mesh)
#' print(tri)
#' plot(tri)
#'
#' # obtain the vertices and indices in raw form
#'
#' ## idx is the triplets of row numbers in tri$vertex
#' idx <- do.call(rbind, sc_object(tri)$topology_)
#' idx <- as.matrix(idx[c(".vx0", ".vx1", ".vx2")])
#'
#' ## vert is the vertices x_, y_, ...
#' vert <- as.matrix(sc_vertex(tri))
#'
#' ## now we can plot with generic tools
#' plot(vert)
#' polygon(vert[t(cbind(idx, NA)), ])
#'
#' ## or create other structures like rgl's mesh3d
#' ## (see hypertidy/anglr for in-dev helpers)
#' ## rgl::tmesh3d(t(cbind(vert, 1, 1)), t(idx),
#' ##   material = list(color = c("firebrick", "black", "grey", "blue")),
#' ##   meshColor = "faces")
TRI0 <- function(x, ...) {
  UseMethod("TRI0")
}

#' @name TRI0
#' @export
TRI0.default <- function(x, ...) {
  TRI0(PATH0(x), ...)
}
#' @name TRI0
#' @export
TRI0.mesh3d <- function(x, ...) {
  index <- x[["it"]]
  if (is.null(index)) {
    index <- .quad2tri(x[["ib"]])
  }
  index <- t(index)
  colnames(index) <- c(".vx0", ".vx1", ".vx2")
  index <- tibble::as_tibble(index)
  object <- tibble::tibble(object_ = 1)  ## we might pull out groups of face material properties?
  v <- t(x[["vb"]])
  colnames(v) <- c("x_", "y_", "z_", "h_")
  vertex <- tibble::as_tibble(v)
  ## might have been a quadmesh
  crs <- NA_character_
  if (!is.null(x[["crs"]]) && !is.na(x[["crs"]])) {
    crs <- x[["crs"]]
  }
  new_TRI0(vertex, object, list(index), crs)

}
#' @name TRI0
#' @export
TRI0.TRI0 <- function(x, ...) {
  x
}
#' @name TRI0
#' @export
TRI0.sfc_TIN <- function(x, ...) {
  stop("TRI0 not implemented for TIN")
}
#' @name TRI0
#' @export
TRI0.TRI <- function(x, ...) {
  o <- sc_object(x)
  o$object_ <- NULL
  topol <- x$triangle
  v <- sc_vertex(x)

  idx <- split(tibble::tibble(.vx0 = match(topol$.vx0, v$vertex_),
                        .vx1 = match(topol$.vx1, v$vertex_),
                        .vx2 = match(topol$.vx2, v$vertex_)),
               topol$object_)[unique(topol$object_)]
  v$vertex_ <- NULL
  crs <- crsmeta::crs_proj(x)
  new_TRI0(v, o, idx, crs, meta = x$meta)

}
#' @name TRI0
#' @export
TRI0.PATH0 <- function(x, ...) {
  v <- sc_vertex(x)
  v$vertex_ <- 1:nrow(v)
  obj <- sc_object(x)
  count <- 0
  trilist <- list()
  for (i in seq_len(nrow(obj))) {
    ## split x$object$path_ on subobject
    topol <- obj$path_[[i]]
    lsubs <- split(topol, topol$subobject)
    ## j is sub polygons
    for (j in seq_along(lsubs)) {
      vidx <- lsubs[[j]]
      verts <- inner_join(vidx[c("vertex_", "path_")], v[c("x_", "y_", "vertex_")], "vertex_")
      ## identify holes (path_ within subobject)
      holes <- which(c(0, abs(diff(as.integer(as.factor(verts$path_))))) > 0)
      if (length(holes) < 1) holes <- 0
      count <- count + 1
      trindex <- decido::earcut(cbind(verts[["x_"]], verts[["y_"]]), holes)
      trimat <- matrix(trindex, ncol = 3L, byrow = TRUE)
      trilist[[count]] <- tibble::tibble(.vx0 = verts$vertex_[trimat[,1L]],
                                         .vx1 = verts$vertex_[trimat[,2L]],
                                         .vx2 = verts$vertex_[trimat[,3L]],
                                         object_ = i,
                                         path_ = lsubs[[j]]$path_[1L])


    }
  }
  ## build TRI0
  obj$path_ <- NULL
  topology_ <- dplyr::bind_rows(trilist)
  index <- split(topology_[c(".vx0", ".vx1",".vx2", "path_")], topology_$object_)
  crs <- crsmeta::crs_proj(x)
  new_TRI0(sc_vertex(x), obj, index, crs, meta = x$meta)
}
#' @name TRI0
#' @export
TRI0.PATH <- function(x, ...) {
  vertex <- x$vertex
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  TRI0(PATH0(x), ...)
}
#' @name TRI0
#' @export
TRI0.sf <- function(x, ...) {
  sfcol <- attr(x, "sf_column")
  out <- TRI0(x[[sfcol]])
  x[[sfcol]] <- NULL
  nm <- names(x)
  for (i in seq_along(x)) {
    ## don't smash the topology or the object
    if (nm[i] %in% c("topology_", "object_")) next;
    out$object[[nm[i]]] <- x[[i]]
  }
  out
}
#' @name TRI0
#' @export
TRI0.sfc_GEOMETRYCOLLECTION <- function(x, ...) {
  list_sfc <- unclass(x)
  lens <- lengths(list_sfc)
  corners <- rapply(list_sfc, function(x) dim(x)[1L], classes = "matrix", how = "unlist")
  if (!all(corners == 4L)) stop("cannot 'TRI0' a GEOMETRYCOLLECTION not composed of triangles (POLYGON with 4 coordinates)")
  ## proceed
  coords <- do.call(rbind, lapply(unlist(list_sfc, recursive = FALSE), "[[", 1L))
  ## every fourth should be redundant
  first <- seq(1, nrow(coords), by = 4L)
  fourth <- first + 3
  if (!max(first) == (nrow(coords)-3) || # doesn't match assumption of 4-plets
      !all(coords[first,1] == coords[fourth, 1]) ||
      !all(coords[first,2] == coords[fourth, 2])) {
    stop("GEOMETRYCOLLECTION appears to not be composed of POLYGON triangles")
  }
  if (ncol(coords) == 2L) {
    coords <- cbind(coords, 0)
  }
  coords <- coords[-fourth, , drop = FALSE]
  colnames(coords) <- c("x_", "y_", "z_")
  topol <- matrix(1:(dim(coords)[1L]), byrow = TRUE, ncol = 3L)
  colnames(topol) <- c(".vx0", ".vx1", ".vx2")
  topol <- tibble::as_tibble(topol)

  ## split based on the lengths from above
  topol <- split(topol, rep(seq_along(lens), lens))

 new_TRI0(tibble::as_tibble(coords), tibble::tibble(object_ = seq_along(lens)),
          topol, crsmeta::crs_proj(x))

}

