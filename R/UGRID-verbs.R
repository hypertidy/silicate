# UGRID verbs ---------------------------------------------------------------
#
# Two layers, both returning plain data frames:
#
# * silicate's sc_* generics (sc_vertex, sc_edge, sc_object, sc_coord) get
#   UGRID methods, so code written against silicate's models works here.
# * meshcore's shared verb set (cells(), boundaries(), vertices(), edges(),
#   face_edge(), neighbours(), join_array(), parent(), children()) gets
#   UGRID methods registered when meshcore is installed. meshcore is a
#   suggested package, so silicate neither imports nor re-exports these.
#
# Keys follow meshcore: .cell, .vx and .edge are 0-based ids (UGRID's
# default start_index). Internally the UGRID tables use 1-based row
# references, as the mesh spec has it for R.

#' @exportS3Method meshcore::cells
cells.UGRID <- function(x, ...) {
  fn <- x$face_node
  cx <- as.vector(tapply(x$node$x[fn$node], fn$face, mean))
  cy <- as.vector(tapply(x$node$y[fn$node], fn$face, mean))
  data.frame(.cell = seq_len(nrow(x$face)) - 1, n = x$face$n, x = cx, y = cy)
}

#' @exportS3Method meshcore::boundaries
boundaries.UGRID <- function(x, ...) {
  fn <- x$face_node
  data.frame(.cell = fn$face - 1, .corner = fn$corner, .vx = fn$node - 1)
}

#' @exportS3Method meshcore::vertices
vertices.UGRID <- function(x, ...) {
  data.frame(.vx = seq_len(nrow(x$node)) - 1, x$node)
}

#' @exportS3Method meshcore::n_cells
n_cells.UGRID <- function(x, ...) nrow(x$face)

# Stored edges: UGRID edge arrays are keyed to the file's edge order, so
# edges() and face_edge() return the edge table the model holds (read
# from edge_node_connectivity, or computed in first-use order).
#' @exportS3Method meshcore::edges
edges.UGRID <- function(x, ...) {
  data.frame(.edge = seq_len(nrow(x$edge)) - 1,
             .vx0 = x$edge$node0 - 1, .vx1 = x$edge$node1 - 1)
}

#' @exportS3Method meshcore::face_edge
face_edge.UGRID <- function(x, ...) {
  fe <- x$face_edge
  structure(data.frame(.cell = fe$face - 1, .corner = fe$corner, .edge = fe$edge - 1),
            edges = edges.UGRID(x))
}

#' @exportS3Method meshcore::as_wkpool
as_wkpool.UGRID <- function(x, ...) {
  ugrid_wkpool(x)
}

#' @exportS3Method meshcore::parent
parent.UGRID <- function(x, ...) {
  stop("UGRID has no hierarchy: a refinement relation would need storing", call. = FALSE)
}

#' @exportS3Method meshcore::children
children.UGRID <- function(x, ...) {
  stop("UGRID has no hierarchy: a refinement relation would need storing", call. = FALSE)
}

#' Join a keyed array to a UGRID model
#'
#' The array's keyed dimension names the location: the model's node,
#' edge or face dimension. Face arrays join to the faces (`.cell`), node
#' arrays to the nodes (`.vx`), edge arrays to the stored edge table
#' (`.edge`, in the file's edge order). Registered as a method of
#' `meshcore::join_array()`.
#'
#' @param x a UGRID model.
#' @param a a keyed array, from [ugrid_read_array()] or
#'   `meshcore::keyed_array()`.
#' @param dim_map named character vector, dimension name to location, for
#'   files that name the dimension differently.
#' @param ... unused.
#' @name join_array.UGRID
#' @exportS3Method meshcore::join_array
join_array.UGRID <- function(x, a, dim_map = NULL, ...) {
  loc <- ugrid_location(x, a$key_dim, dim_map)
  tab <- switch(loc,
    face = cells.UGRID(x),
    node = vertices.UGRID(x),
    edge = data.frame(.edge = seq_len(nrow(x$edge)) - 1,
                      .vx0 = x$edge$node0 - 1, .vx1 = x$edge$node1 - 1))
  n <- a$dims[[a$key_dim]]
  if (n != nrow(tab)) {
    stop(sprintf("%s has %d entries, the mesh's %s dimension (%s) has %d",
                 a$key_dim, n, loc, x$meta$dims[[loc]], nrow(tab)), call. = FALSE)
  }
  kd <- match(a$key_dim, names(a$dims))
  v <- aperm(array(a$values, a$dims), c(kd, seq_along(a$dims)[-kd]))
  m <- matrix(v, nrow = n)
  idx <- if (is.null(a$key)) seq_len(n) else match(seq_len(n) - 1, a$key)
  meshcore::long_join(tab, idx, m, a)
}

ugrid_location <- function(x, dim, dim_map = NULL) {
  d <- x$meta$dims
  key <- stats::setNames(names(d), d)
  key <- key[!is.na(names(key))]
  if (!is.null(dim_map)) key[names(dim_map)] <- dim_map
  loc <- key[dim]
  if (is.na(loc)) stop("dimension ", dim, " is not this mesh's node, edge or face dimension",
                       " (", paste(names(key), collapse = ", "), "); use dim_map", call. = FALSE)
  unname(loc)
}

#' Keyed dimensions of a UGRID model
#'
#' @param x a UGRID model.
#' @return data.frame: location, the netCDF dimension name it is keyed
#'   to, and its length.
#' @export
mesh_dims <- function(x) {
  d <- x$meta$dims
  data.frame(location = c("node", "edge", "face"),
             dim = unname(d[c("node", "edge", "face")]),
             n = c(nrow(x$node), nrow(x$edge), nrow(x$face)))
}

#' Edges on the mesh boundary
#'
#' @param x a UGRID model.
#' @return the stored edges with a face on one side only: .edge, .vx0,
#'   .vx1 (0-based ids) and the face on each side (.left, .right; NA
#'   outside).
#' @export
boundary_edges <- function(x) {
  ef <- x$edge_face
  i <- which(is.na(ef$left) | is.na(ef$right))
  data.frame(.edge = i - 1, .vx0 = x$edge$node0[i] - 1, .vx1 = x$edge$node1[i] - 1,
             .left = ef$left[i] - 1, .right = ef$right[i] - 1)
}

#' @export
sc_vertex.UGRID <- function(x, ...) {
  data.frame(x_ = x$node$x, y_ = x$node$y, vertex_ = seq_len(nrow(x$node)))
}
#' @export
sc_edge.UGRID <- function(x, ...) {
  data.frame(edge_ = seq_len(nrow(x$edge)),
             .vx0 = x$edge$node0, .vx1 = x$edge$node1)
}
#' @export
sc_object.UGRID <- function(x, ...) {
  # the objects of a UGRID are its faces (or edges, for a 1D network)
  if (nrow(x$face)) data.frame(object_ = seq_len(nrow(x$face)), x$face)
  else sc_edge(x)
}
#' @export
sc_coord.UGRID <- function(x, ...) {
  # coordinates in face-corner order, as silicate's sc_coord gives path order
  data.frame(x_ = x$node$x[x$face_node$node], y_ = x$node$y[x$face_node$node])
}
