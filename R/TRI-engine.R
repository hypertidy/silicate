## TRI by constrained Delaunay triangulation (silicate2)
##
## Each polygon (object_, subobject) is triangulated as a PSLG: every ring
## edge is a constraint and triangles at even depth (outside, or in a hole)
## are erased. The engine is cdtr or laridae through meshcore::tri_mesh(),
## which returns the mesh spec v0.1 triangle form. Ring orientation is not
## used: holes come from depth, so clockwise shells are fine.

tri_engine <- function(engine = getOption("silicate.tri_engine", "auto")) {
  engine <- match.arg(engine, c("auto", "cdtr", "laridae", "earcut"))
  have <- function(pkg) requireNamespace(pkg, quietly = TRUE)
  if (engine == "auto") {
    engine <- if (have("meshcore") && have("cdtr")) "cdtr" else "earcut"
  } else if (engine != "earcut") {
    for (pkg in c("meshcore", engine)) {
      if (!have(pkg)) stop(sprintf("TRI engine \"%s\" needs the '%s' package", engine, pkg))
    }
  }
  engine
}

## The polygons of a PATH, as in triangulate_0(): one data frame of
## path_link_vertex joined to vertex per (object_, subobject), in path order.
path_polygons <- function(x) {
  path <- x$path
  key <- paste(path$object_, path$subobject, sep = "\r")
  verts <- dplyr::inner_join(x$path_link_vertex, x$vertex, "vertex_")
  verts <- split(verts, factor(verts$path_, levels = unique(verts$path_)))
  polys <- split(path$path_, factor(key, levels = unique(key)))
  lapply(polys, function(p) {
    out <- dplyr::bind_rows(verts[p])
    attr(out, "object_") <- path$object_[match(p[1L], path$path_)]
    out
  })
}

triangulate_engine <- function(x, engine, ...) {
  vertex <- x$vertex
  attr_cols <- setdiff(names(vertex)[vapply(vertex, is.numeric, logical(1))], c("x_", "y_"))
  polys <- path_polygons(x)
  trilist <- vector("list", length(polys))
  newvert <- vector("list", length(polys))
  for (i in seq_along(polys)) {
    v <- polys[[i]]
    n <- nrow(v)
    ## each ring closes on itself; drop the zero-length closing segment that a
    ## repeated first vertex would give
    s0 <- seq_len(n)
    s1 <- s0 + 1L
    last <- !duplicated(v$path_, fromLast = TRUE)
    first <- which(!duplicated(v$path_))
    s1[last] <- first[match(v$path_[last], v$path_[first])]
    keep <- v$vertex_[s0] != v$vertex_[s1]
    PA <- if (length(attr_cols)) as.matrix(v[attr_cols]) else NULL
    m <- meshcore::tri_mesh(v$x_, v$y_, s0[keep], s1[keep], PA = PA,
                            engine = engine, erase = "holes", ...)
    mv <- m$vertices
    ## vertex_ for each mesh vertex row: input rows map back through the
    ## engine's input map, new (crossing, Steiner) vertices get new ids
    vid <- rep(NA_character_, nrow(mv))
    im <- attr(m, "input_map")
    vid[im] <- v$vertex_
    new <- is.na(vid)
    if (any(new)) {
      vid[new] <- sc_uid(sum(new))
      nv <- tibble::tibble(x_ = mv$x[new], y_ = mv$y[new])
      for (a in attr_cols) nv[[a]] <- mv[[a]][new]
      nv$vertex_ <- vid[new]
      newvert[[i]] <- nv
    }
    t <- m$triangles
    trilist[[i]] <- tibble::tibble(.vx0 = vid[t$v0], .vx1 = vid[t$v1], .vx2 = vid[t$v2],
                                   object_ = attr(v, "object_"))
  }
  list(triangle = dplyr::bind_rows(trilist), vertex = dplyr::bind_rows(vertex, dplyr::bind_rows(newvert)))
}
