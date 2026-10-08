# The zoo -----------------------------------------------------------------
#
# The zookeeper's job: a registry of models (tables, keys, axes, identity
# rung), validation of a model against its spec, and the conversion
# routes between models, with what each route loses. Every missing
# route or failed check is a named gap.

#' The model registry
#'
#' One row per model in the zoo, with the four axes from the model zoo
#' design (primitive, stored or computed vertices, ordering, identity)
#' and whether this prototype implements it.
#' @export
zoo_models <- function() {
  data.frame(
    model = c("UGRID", "GRID", "CELL", "SC", "PATH", "ARC", "TRI", "QUAD",
              "FACE", "HALFEDGE", "SIMPLEX", "TRACK", "GRAPH"),
    primitive = c("node, edge, face (mixed)", "quad cell", "cell id",
                  "segment", "path", "arc", "triangle", "quad",
                  "polygon face with holes", "half-edge", "k-simplex",
                  "path with time", "directed weighted edge"),
    vertices = c("stored", "computed", "computed", "stored", "stored",
                 "stored", "stored", "stored", "stored", "stored", "stored",
                 "stored", "stored"),
    ordering = c("structural", "structural (implicit)", "structural (implicit)",
                 "structural", "sequential", "sequential", "structural",
                 "structural", "sequential cycles", "structural", "structural",
                 "sequential", "structural"),
    tables = c("node, face_node, edge, face_edge, edge_face",
               "descriptor (extent, dimension)", "cell ids + descriptor (scheme, level)",
               "object, edge, vertex", "object, path, path_link_vertex, vertex",
               "object, arc_link_vertex, vertex", "object, triangle, vertex",
               "quad, vertex", "face, cycle, vertex", "halfedge, vertex",
               "simplex, vertex", "track, vertex", "edge, vertex"),
    implemented = c("silicate (this branch)", "meshcore (GRID/CELL thread)",
                    "meshcore (GRID/CELL thread)", "silicate", "silicate",
                    "silicate; wkpool arcs", "silicate; engines", "anglr",
                    "wkpool cycles (nearly)", "-", "-", "-", "-"),
    stringsAsFactors = FALSE
  )
}

#' Validate a model against its spec
#'
#' @param x a model.
#' @param ... unused.
#' @return data.frame of checks: check, ok, value, note.
#' @export
zoo_validate <- function(x, ...) UseMethod("zoo_validate")

#' @export
zoo_validate.UGRID <- function(x, ...) {
  nn <- nrow(x$node)
  fn <- x$face_node
  chk <- list()
  add <- function(check, ok, value, note = "") {
    chk[[length(chk) + 1L]] <<- data.frame(check = check, ok = ok,
                                           value = as.character(value), note = note)
  }
  add("node coordinates finite", all(is.finite(x$node$x) & is.finite(x$node$y)),
      sum(!is.finite(x$node$x) | !is.finite(x$node$y)))
  add("edge nodes in range", all(x$edge$node0 %in% seq_len(nn) & x$edge$node1 %in% seq_len(nn)),
      sum(!(x$edge$node0 %in% seq_len(nn) & x$edge$node1 %in% seq_len(nn))))
  add("no degenerate edges", all(x$edge$node0 != x$edge$node1), sum(x$edge$node0 == x$edge$node1))
  if (!is.null(fn) && nrow(fn)) {
    add("face nodes in range", all(fn$node %in% seq_len(nn)), sum(!fn$node %in% seq_len(nn)))
    add("faces have 3+ corners", all(x$face$n >= 3L), sum(x$face$n < 3L))
    rep_node <- duplicated(fn[c("face", "node")])
    add("no repeated node in a face", !any(rep_node), sum(rep_node))
    miss <- attr(x$face_edge, "missing") %||% 0L
    add("face sides found in edge table", miss == 0L, miss,
        if (x$meta$edge_source == "stored") "stored edges checked against faces" else "")
    sc <- attr(x$edge_face, "side_count")
    nonman <- sum(sc[, "left"] > 1L | sc[, "right"] > 1L)
    add("manifold (one face per edge side)", nonman == 0L, nonman,
        "two faces on one side means a flipped face or an overlap")
    unused <- setdiff(seq_len(nrow(x$edge)), x$face_edge$edge)
    add("every edge bounds a face", length(unused) == 0L, length(unused),
        "edges outside faces are allowed (1D/2D mixed meshes)")
    ne <- length(unique(x$face_edge$edge))
    chi <- nrow(x$node) - ne + nrow(x$face)
    add("Euler characteristic V - E + F", TRUE, chi,
        "1 for a disc, 2 for a closed sphere, 1 - holes for a disc with holes")
    a <- face_signed_area(x)
    add("faces counter-clockwise", all(a > 0), sum(a <= 0),
        "UGRID says anticlockwise; lon/lat faces across the antimeridian show up here")
    lonlat <- x$meta$crs %in% c("OGC:CRS84", "EPSG:4326")
    if (lonlat) {
      span <- tapply(x$node$x[fn$node], fn$face, function(v) diff(range(v)))
      add("no face spans more than 180 degrees of longitude", all(span <= 180), sum(span > 180),
          "planar verbs need these cut or unwrapped")
    }
  }
  out <- do.call(rbind, chk)
  out
}

face_signed_area <- function(x) {
  fn <- x$face_node
  fe <- x$face_edge
  X <- x$node$x; Y <- x$node$y
  b <- ifelse(fe$forward, x$edge$node1[fe$edge], x$edge$node0[fe$edge])
  a <- fn$node
  as.vector(tapply(X[a] * Y[b] - X[b] * Y[a], fn$face, sum)) / 2
}

#' The conversion routes
#'
#' One row per registered route between models, with whether it keeps
#' everything. Routes not listed are gaps.
#' @export
zoo_routes <- function() {
  r <- function(from, to, via, loses) data.frame(from = from, to = to, via = via, loses = loses)
  rbind(
    r("UGRID", "TRI", "as_tri()", "nothing for convex faces; face kept per triangle"),
    r("UGRID", "drawable", "as_drawable()", "nothing (float32 at serialisation)"),
    r("UGRID", "mesh3d", "as_mesh3d()", "nothing; quads stay quads"),
    r("UGRID", "wkpool", "meshcore::as_wkpool() (method here)", "nothing; one path per face"),
    r("UGRID", "polygons (wk)", "ugrid_polygons()", "shared edges (re-derivable by wkpool)"),
    r("UGRID", "SC", "sc_vertex()/sc_edge()/sc_object()", "corner order lives in face_node, not SC"),
    r("UGRID", "netCDF", "write_ugrid()", "nothing for 1D/2D"),
    r("netCDF", "UGRID", "read_ugrid()", "nothing for 1D/2D; 3D layered and mixed 1D2D read as separate meshes"),
    r("polygons (wk)", "UGRID", "as_UGRID()", "holes (UGRID faces are simple)"),
    r("lines (wk)", "UGRID", "as_UGRID()", "line identity (edges only)"),
    r("wkpool", "UGRID", "as_UGRID()", "holes"),
    r("TRI", "UGRID", "as_UGRID()", "nothing"),
    r("GRID", "UGRID", "as_UGRID(meshcore::GRID())", "nothing; vertices go from computed to stored"),
    r("CELL", "UGRID", "as_UGRID(meshcore::CELL())", "hierarchy (parent/children); spherical edges become planar")
  )
}

#' The conversion matrix
#'
#' @return character matrix, rows from, columns to: "lossless", "lossy",
#'   or "" for a missing route.
#' @export
zoo_matrix <- function() {
  r <- zoo_routes()
  lev <- unique(c(r$from, r$to))
  m <- matrix("", length(lev), length(lev), dimnames = list(from = lev, to = lev))
  m[cbind(r$from, r$to)] <- ifelse(grepl("^nothing", r$loses), "lossless", "lossy")
  m
}
