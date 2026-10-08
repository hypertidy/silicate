# UGRID model -----------------------------------------------------------
#
# A UGRID model is a set of keyed tables. Every reference column holds a
# row position (1-based in R) in another table, and each table records
# the netCDF dimension it is keyed to, so an array defined on that
# dimension joins to the table row for row.
#
#   node       one row per node: x, y (+ any node attributes)
#   face       one row per face: n (corner count)
#   face_node  one row per face corner: face, corner, node (stored order)
#   edge       one row per edge: node0, node1
#   face_edge  one row per face corner: face, corner, edge, forward
#   edge_face  one row per edge: left, right (face rows; NA on a boundary)
#   meta       mesh name, topology dimension, keyed dimension names,
#              start index read, crs, where edges came from, identity rung
#
# Only node and face_node are required. edge, face_edge and edge_face are
# computed from face_node when the source does not store them, and the
# meta records which.

#' Build a UGRID model from node coordinates and face corners
#'
#' @param node data.frame with columns x and y (one row per node).
#' @param face_node data.frame with columns face, corner, node (1-based
#'   rows), or an integer matrix with one row per face and NA padding.
#' @param edge optional data.frame with columns node0, node1.
#' @param meta optional list, see Details.
#' @export
UGRID <- function(node, face_node = NULL, edge = NULL, meta = list()) {
  stopifnot(is.data.frame(node), all(c("x", "y") %in% names(node)))
  if (is.matrix(face_node)) face_node <- face_node_long(face_node)
  meta <- utils::modifyList(list(
    mesh = "mesh2d",
    topology_dimension = if (is.null(face_node)) 1L else 2L,
    dims = c(node = "n_node", edge = "n_edge", face = "n_face"),
    start_index = 0L,
    crs = NA_character_,
    edge_source = if (is.null(edge)) "computed" else "stored",
    identity = "stored"
  ), meta)
  x <- list(node = node, face_node = face_node, edge = edge, meta = meta)
  x <- ugrid_complete(x)
  structure(x, class = c("UGRID", "sc"))
}

# padded face x corner matrix -> long face_node table
face_node_long <- function(m) {
  storage.mode(m) <- "integer"
  nf <- nrow(m)
  face <- rep(seq_len(nf), ncol(m))
  corner <- rep(seq_len(ncol(m)), each = nf)
  node <- as.vector(m)
  keep <- !is.na(node)
  out <- data.frame(face = face[keep], corner = corner[keep], node = node[keep])
  out <- out[order(out$face, out$corner), , drop = FALSE]
  # renumber corners so a face with interior fill values stays contiguous
  out$corner <- sequence(tabulate(out$face, nf))
  row.names(out) <- NULL
  out
}

# long face_node table -> padded face x corner matrix
face_node_matrix <- function(x) {
  fn <- x$face_node
  nf <- nrow(x$face)
  m <- matrix(NA_integer_, nf, max(x$face$n))
  m[cbind(fn$face, fn$corner)] <- fn$node
  m
}

# Fill in face, edge, face_edge and edge_face from node, face_node, edge.
ugrid_complete <- function(x) {
  nn <- nrow(x$node)
  fn <- x$face_node
  if (is.null(fn)) {
    x$face <- data.frame(n = integer())
    if (is.null(x$edge)) stop("a 1D UGRID needs an edge table", call. = FALSE)
    x$edge <- x$edge[c("node0", "node1")]
    return(x)
  }
  fn <- fn[order(fn$face, fn$corner), c("face", "corner", "node")]
  row.names(fn) <- NULL
  nf <- if (nrow(fn)) max(fn$face) else 0L
  x$face_node <- fn
  x$face <- data.frame(n = tabulate(fn$face, nf))

  # each corner goes to the next corner of the same face, the last wraps
  i <- seq_len(nrow(fn))
  first <- !duplicated(fn$face)
  last <- c(first[-1L], TRUE)
  nxt <- i + 1L
  nxt[last] <- which(first)[cumsum(first)][last]
  a <- fn$node
  b <- fn$node[nxt]
  lo <- pmin(a, b)
  hi <- pmax(a, b)
  key <- lo * (nn + 1) + hi  # double: exact up to ~9e15

  if (is.null(x$edge)) {
    ukey <- unique(key)
    x$edge <- data.frame(node0 = as.integer(ukey %/% (nn + 1)),
                         node1 = as.integer(ukey %% (nn + 1)))
  }
  e0 <- x$edge$node0
  e1 <- x$edge$node1
  ekey <- pmin(e0, e1) * (nn + 1) + pmax(e0, e1)
  edge <- match(key, ekey)
  # forward: the face walks this edge from node0 to node1, so the face
  # is on the edge's left when faces are counter-clockwise
  forward <- a == e0[edge]
  x$face_edge <- data.frame(face = fn$face, corner = fn$corner,
                            edge = edge, forward = forward)
  ne <- nrow(x$edge)
  ok <- !is.na(edge)
  left <- rep(NA_integer_, ne)
  right <- rep(NA_integer_, ne)
  fl <- ok & forward
  fr <- ok & !forward
  left[edge[fl]] <- fn$face[fl]
  right[edge[fr]] <- fn$face[fr]
  x$edge_face <- data.frame(left = left, right = right)
  # tallies for validation (more than one face on a side = non-manifold)
  attr(x$edge_face, "side_count") <- cbind(
    left = tabulate(edge[fl], ne), right = tabulate(edge[fr], ne))
  attr(x$face_edge, "missing") <- sum(!ok)
  x
}

#' @export
print.UGRID <- function(x, ...) {
  m <- x$meta
  cat("UGRID model", sQuote(m$mesh), " topology_dimension:", m$topology_dimension, "\n")
  cat(sprintf("  nodes %d [%s]  edges %d [%s, %s]  faces %d [%s]\n",
              nrow(x$node), m$dims[["node"]],
              nrow(x$edge), m$dims[["edge"]], m$edge_source,
              nrow(x$face), m$dims[["face"]]))
  if (nrow(x$face)) {
    tab <- table(x$face$n)
    cat("  corners per face:", paste0(names(tab), ":", tab, collapse = " "), "\n")
  }
  cat("  crs:", if (is.na(m$crs)) "(none)" else m$crs,
      "  identity:", m$identity, "\n")
  invisible(x)
}
