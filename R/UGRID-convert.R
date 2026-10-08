# Conversions between UGRID and the rest of the zoo ------------------------

#' Convert to a UGRID model
#'
#' Methods: a wkpool (faces from its cycles, or edges when it has none);
#' a meshcore model (GRID, CELL);
#' anything wk can handle (polygons or lines, through wkpool at identity
#' rung "exact"); a meshspec triangle form (list with vertices and
#' triangles); a grid descriptor from [grid_descriptor()].
#'
#' @param x input.
#' @param ... passed on.
#' @export
as_UGRID <- function(x, ...) UseMethod("as_UGRID")

#' @export
as_UGRID.UGRID <- function(x, ...) x

crs_string <- function(crs) {
  if (is.null(crs) || inherits(crs, "wk_crs_inherit")) return(NA_character_)
  if (is.character(crs) && length(crs) == 1L) return(crs)
  out <- tryCatch(wk::wk_crs_proj_definition(crs), error = function(e) NULL)
  if (is.null(out) || !nzchar(out)) NA_character_ else out
}

#' @param tolerance passed to `wkpool::merge_coincident()`.
#' @rdname as_UGRID
#' @export
as_UGRID.default <- function(x, tolerance = 0, ...) {
  check_suggested("wkpool")
  pool <- wkpool::establish_topology(x)
  pool <- wkpool::merge_coincident(pool, tolerance = tolerance)
  out <- as_UGRID(pool, ...)
  out$meta$identity <- if (tolerance > 0) "snapped" else "exact"
  out
}

#' @rdname as_UGRID
#' @export
as_UGRID.wkpool <- function(x, ...) {
  pool <- wkpool::pool_vertices(x)
  crs <- crs_string(wk::wk_crs(x))
  cycles <- wkpool::find_cycles(x)
  if (length(cycles)) {
    cls <- wkpool::classify_cycles(x)
    outer <- cls$type == "outer"
    lost <- sum(!outer)
    cycles <- cycles[outer]
    # counter-clockwise faces, the UGRID convention
    cw <- cls$area[outer] < 0
    cycles[cw] <- lapply(cycles[cw], rev)
    used <- sort(unique(unlist(cycles)))
    node <- pool[match(used, pool$.vx), c("x", "y")]
    row.names(node) <- NULL
    fn <- data.frame(face = rep(seq_along(cycles), lengths(cycles)),
                     corner = sequence(lengths(cycles)),
                     node = match(unlist(cycles), used))
    out <- UGRID(node, fn, meta = list(crs = crs))
    if (!is.null(cls$.feature)) out$face$feature <- cls$.feature[outer]
    out$meta$lost <- c(holes = lost)
    return(out)
  }
  # no closed cycles: a 1D network of the distinct undirected edges
  s <- wkpool::pool_segments(x)
  lo <- pmin(s$.vx0, s$.vx1); hi <- pmax(s$.vx0, s$.vx1)
  k <- !duplicated(cbind(lo, hi)) & lo != hi
  used <- sort(unique(c(lo[k], hi[k])))
  node <- pool[match(used, pool$.vx), c("x", "y")]
  row.names(node) <- NULL
  edge <- data.frame(node0 = match(lo[k], used), node1 = match(hi[k], used))
  UGRID(node, NULL, edge, meta = list(crs = crs, topology_dimension = 1L,
                                      edge_source = "stored"))
}

#' @rdname as_UGRID
#' @export
as_UGRID.meshspec_tri <- function(x, ...) {
  tr <- x$triangles
  m <- cbind(tr$v0, tr$v1, tr$v2)
  out <- UGRID(x$vertices[c("x", "y")], m, meta = list(crs = x$crs %||% NA_character_))
  extra <- setdiff(names(tr), c("v0", "v1", "v2"))
  for (nm in extra) out$face[[nm]] <- tr[[nm]]
  out
}

#' A regular grid descriptor
#'
#' The GRID model in its smallest form: an extent and a dimension, from
#' which every vertex and cell is computed. Cells are numbered as GDAL
#' numbers pixels, row-major from the top left.
#'
#' @param extent xmin, xmax, ymin, ymax.
#' @param dimension ncol, nrow.
#' @param crs optional crs string.
#' @export
grid_descriptor <- function(extent, dimension, crs = NA_character_) {
  structure(list(extent = as.numeric(extent), dimension = as.integer(dimension),
                 crs = crs), class = "grid_descriptor")
}

#' @rdname as_UGRID
#' @export
as_UGRID.grid_descriptor <- function(x, ...) {
  nc <- x$dimension[1L]; nr <- x$dimension[2L]
  e <- x$extent
  dx <- (e[2L] - e[1L]) / nc
  dy <- (e[4L] - e[3L]) / nr
  # nodes: (nr + 1) rows of (nc + 1), row-major from the top left
  cc <- rep(0:nc, times = nr + 1L)
  rr <- rep(0:nr, each = nc + 1L)
  node <- data.frame(x = e[1L] + cc * dx, y = e[4L] - rr * dy)
  id <- function(r, c) r * (nc + 1L) + c + 1L
  fc <- rep(0:(nc - 1L), times = nr)
  fr <- rep(0:(nr - 1L), each = nc)
  # top-left, bottom-left, bottom-right, top-right: counter-clockwise
  m <- cbind(id(fr, fc), id(fr + 1L, fc), id(fr + 1L, fc + 1L), id(fr, fc + 1L))
  UGRID(node, m, meta = list(crs = x$crs,
                             dims = c(node = "n_node", edge = "n_edge", face = "n_cell"),
                             identity = "computed"))
}

#' Faces as polygons
#'
#' @param x a UGRID model.
#' @return a wk wkb vector, one polygon per face.
#' @export
ugrid_polygons <- function(x) {
  check_suggested("wk")
  fn <- x$face_node
  out <- wk::wk_polygon(wk::xy(x$node$x[fn$node], x$node$y[fn$node]),
                        feature_id = fn$face)
  if (!is.na(x$meta$crs)) out <- wk::wk_set_crs(out, x$meta$crs)
  out
}

# Nodes become the pool's vertices (.vx = node row) and each face one closed
# path of segments (.feature = .path = face row), so wkpool's verbs work on
# the mesh directly. A 1D mesh gives one segment per edge.
ugrid_wkpool <- function(x) {
  check_suggested("wkpool")
  v <- data.frame(.vx = seq_len(nrow(x$node)), x = x$node$x, y = x$node$y)
  crs <- if (is.na(x$meta$crs)) NULL else x$meta$crs
  if (!nrow(x$face)) {
    return(wkpool::new_wkpool(v, x$edge$node0, x$edge$node1,
                              feature = seq_len(nrow(x$edge)), crs = crs))
  }
  fn <- x$face_node
  fe <- x$face_edge
  e <- x$edge
  b <- ifelse(fe$forward, e$node1[fe$edge], e$node0[fe$edge])
  nf <- nrow(x$face)
  paths <- data.frame(.path = seq_len(nf), .feature = seq_len(nf),
                      .part = 1L, .ring = 1L)
  wkpool::new_wkpool(v, fn$node, b, feature = fn$face, path = fn$face,
                     paths = paths, crs = crs)
}

#' Triangulate faces into the mesh spec triangle form
#'
#' Fan triangulation from each face's first corner: exact for convex
#' faces (all of UGRID's common triangle, quad and hexagon meshes).
#' Each triangle keeps the face row it came from, so face arrays still
#' join. The "fan_ok" attribute counts triangles with positive signed
#' area; fewer than all means a non-convex or clockwise face.
#'
#' @param x a UGRID model.
#' @export
as_tri <- function(x) {
  fn <- x$face_node
  n <- x$face$n[fn$face]
  first <- !duplicated(fn$face)
  start <- which(first)[cumsum(first)]
  k <- fn$corner >= 2L & fn$corner <= n - 1L
  i <- which(k)
  tri <- data.frame(v0 = fn$node[start[i]], v1 = fn$node[i], v2 = fn$node[i + 1L],
                    face = fn$face[i])
  X <- x$node$x; Y <- x$node$y
  area <- ((X[tri$v1] - X[tri$v0]) * (Y[tri$v2] - Y[tri$v0]) -
           (X[tri$v2] - X[tri$v0]) * (Y[tri$v1] - Y[tri$v0])) / 2
  structure(list(vertices = x$node[c("x", "y")], triangles = tri,
                 crs = x$meta$crs),
            class = "meshspec_tri", fan_ok = sum(area > 0))
}

#' The mesh spec drawable form
#'
#' @param x a UGRID model.
#' @return list: vertices (x, y as float32-ready doubles), indices
#'   (0-based, three per triangle) and faces (the source face row per
#'   triangle, for colouring by a face array).
#' @export
as_drawable <- function(x) {
  t <- as_tri(x)$triangles
  list(vertices = x$node[c("x", "y")],
       indices = as.integer(t(as.matrix(t[c("v0", "v1", "v2")]))) - 1L,
       faces = data.frame(face = t$face),
       crs = x$meta$crs)
}

#' rgl mesh3d, quads kept as quads
#'
#' @param x a UGRID model.
#' @param z optional node heights.
#' @export
as_mesh3d <- function(x, z = 0) {
  fm <- face_node_matrix(x)
  n <- x$face$n
  quad <- n == 4L
  tri_faces <- as_tri(x)$triangles
  tri_faces <- tri_faces[!quad[tri_faces$face], ]
  structure(list(
    vb = rbind(x$node$x, x$node$y, rep_len(z, nrow(x$node)), 1),
    ib = if (any(quad)) t(fm[quad, 1:4, drop = FALSE]) else NULL,
    it = if (nrow(tri_faces)) t(as.matrix(tri_faces[c("v0", "v1", "v2")])) else NULL,
    material = list()),
    face = list(quad = which(quad), tri = tri_faces$face),
    class = c("mesh3d", "shape3d"))
}

#' Orient every face counter-clockwise
#'
#' UGRID asks for anticlockwise faces, and some producers (FESOM) store
#' them clockwise. Planar signed area decides, so faces of a lon/lat mesh
#' that cross the antimeridian are left alone and counted in the
#' "skipped" attribute.
#'
#' @param x a UGRID model.
#' @export
ugrid_orient <- function(x) {
  a <- face_signed_area(x)
  skip <- integer()
  if (x$meta$crs %in% c("OGC:CRS84", "EPSG:4326")) {
    span <- tapply(x$node$x[x$face_node$node], x$face_node$face, function(v) diff(range(v)))
    skip <- which(span > 180)
  }
  flip <- setdiff(which(a < 0), skip)
  if (length(flip)) {
    fn <- x$face_node
    i <- fn$face %in% flip
    fn$corner[i] <- x$face$n[fn$face[i]] - fn$corner[i] + 1L
    x <- UGRID(x$node, fn, if (x$meta$edge_source == "stored") x$edge, x$meta)
  }
  attr(x, "flipped") <- length(flip)
  attr(x, "skipped") <- length(skip)
  x
}

#' @rdname as_UGRID
#' @details Any meshcore model (GRID, CELL) converts through its
#'   `boundaries()` and `vertices()` alone: cell ids keep their order as
#'   face rows, so an array keyed to the model's cells joins to the UGRID
#'   faces unchanged.
#' @export
as_UGRID.meshcore_model <- function(x, ...) {
  check_suggested("meshcore")
  b <- meshcore::boundaries(x)
  v <- meshcore::vertices(x)
  node <- v[c("x", "y")]
  fn <- data.frame(face = match(b$.cell, unique(b$.cell)), corner = b$.corner,
                   node = match(b$.vx, v$.vx))
  out <- UGRID(node, fn, meta = list(
    crs = crs_string(x$meta$crs),
    dims = c(node = "n_node", edge = "n_edge", face = "n_cell"),
    identity = x$meta$rung %||% "lattice"))
  out$face$.cell <- unique(b$.cell)
  out
}
