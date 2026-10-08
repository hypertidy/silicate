# UGRID netCDF I/O ---------------------------------------------------------

nc_att <- function(nc, var, name) {
  tryCatch(RNetCDF::att.get.nc(nc, var, name), error = function(e) NULL)
}

nc_var_dims <- function(nc, var) {
  ids <- RNetCDF::var.inq.nc(nc, var)$dimids
  ids <- ids[!is.na(ids)]
  vapply(ids, function(i) RNetCDF::dim.inq.nc(nc, i)$name, "")
}

nc_dim_len <- function(nc, name) {
  RNetCDF::dim.inq.nc(nc, name)$length
}

nc_var_names <- function(nc) {
  n <- RNetCDF::file.inq.nc(nc)$nvars
  vapply(seq_len(n) - 1L, function(i) RNetCDF::var.inq.nc(nc, i)$name, "")
}

split_words <- function(x) strsplit(trimws(x), "[[:space:]]+")[[1]]

#' Find the mesh topology variables in a netCDF file
#'
#' @param dsn path to a netCDF file.
#' @return data.frame with one row per variable whose cf_role is
#'   "mesh_topology": its name and topology_dimension.
#' @export
ugrid_meshes <- function(dsn) {
  check_suggested("RNetCDF")
  nc <- RNetCDF::open.nc(dsn)
  on.exit(RNetCDF::close.nc(nc))
  v <- nc_var_names(nc)
  role <- vapply(v, function(i) {
    r <- nc_att(nc, i, "cf_role")
    if (is.null(r)) "" else r
  }, "")
  v <- v[role == "mesh_topology"]
  td <- vapply(v, function(i) as.integer(nc_att(nc, i, "topology_dimension") %||% NA), 1L)
  data.frame(mesh = v, topology_dimension = td, row.names = NULL)
}

check_suggested <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("package '", pkg, "' is needed for this (install it to use UGRID)", call. = FALSE)
  }
}

# Read an index connectivity variable as a (entity x k) integer matrix of
# 1-based rows, with NA for fill. `entity_dim` names the dimension that
# indexes the entity (faces or edges); `n_target` is the count of the
# referenced table, used to infer a missing start_index.
read_connectivity <- function(nc, var, entity_dim, n_target) {
  dims <- nc_var_dims(nc, var)
  m <- RNetCDF::var.get.nc(nc, var, collapse = FALSE)
  m <- matrix(as.numeric(m), dim(m)[1L], dim(m)[2L])
  # RNetCDF reports dimensions in R order (first index fastest)
  rdims <- dims
  if (is.null(entity_dim) || !entity_dim %in% dims) {
    # no face_dimension: take the longer dimension as the entity
    entity_dim <- rdims[which.max(dim(m))]
  }
  if (rdims[1L] != entity_dim) m <- t(m)
  start <- nc_att(nc, var, "start_index")
  inferred <- is.null(start)
  fill <- nc_att(nc, var, "_FillValue")
  if (!is.null(fill)) m[m == fill] <- NA
  if (inferred) {
    rng <- range(m, na.rm = TRUE)
    start <- if (rng[1L] == 0) 0 else if (rng[2L] == n_target) 1 else 0
  }
  m[m < start] <- NA  # negative fill values not declared as _FillValue
  m <- m - start + 1
  storage.mode(m) <- "integer"
  structure(m, start_index = as.integer(start), inferred = inferred,
            entity_dim = entity_dim)
}

crs_from_nc <- function(nc, node_vars) {
  # a grid_mapping on a coordinate, else a variable carrying an epsg code
  for (v in node_vars) {
    gm <- nc_att(nc, v, "grid_mapping")
    if (!is.null(gm)) {
      code <- nc_att(nc, gm, "epsg") %||% nc_att(nc, gm, "EPSG_code")
      code <- gsub("[^0-9]", "", code)
      if (length(code) && nzchar(code) && code != "0") return(paste0("EPSG:", code))
      wkt <- nc_att(nc, gm, "crs_wkt") %||% nc_att(nc, gm, "spatial_ref")
      if (!is.null(wkt) && nzchar(wkt)) return(wkt)
    }
  }
  for (v in nc_var_names(nc)) {
    code <- nc_att(nc, v, "epsg")
    if (!is.null(code) && is.numeric(code) && code > 0) return(paste0("EPSG:", code))
  }
  sn <- vapply(node_vars, function(v) nc_att(nc, v, "standard_name") %||% "", "")
  if (all(c("longitude", "latitude") %in% sn)) return("OGC:CRS84")
  NA_character_
}

#' Read a UGRID mesh from netCDF
#'
#' Reads the mesh topology variable and its node coordinates, face-node
#' and (when stored) edge-node connectivity. Index variables in either
#' dimension order, with 0- or 1-based start_index and fill values, are
#' normalised to 1-based rows. Data variables are not read: see
#' [ugrid_arrays()] and [ugrid_read_array()].
#'
#' @param dsn path to a netCDF file.
#' @param mesh name of the mesh topology variable; the first one found
#'   when NULL.
#' @return a UGRID model.
#' @export
read_ugrid <- function(dsn, mesh = NULL) {
  meshes <- ugrid_meshes(dsn)
  if (!nrow(meshes)) stop("no variable with cf_role = \"mesh_topology\" in ", dsn, call. = FALSE)
  if (is.null(mesh)) mesh <- meshes$mesh[1L]
  nc <- RNetCDF::open.nc(dsn)
  on.exit(RNetCDF::close.nc(nc))
  a <- function(name) nc_att(nc, mesh, name)

  node_vars <- split_words(a("node_coordinates"))
  # order x then y: longitude / projection_x first, else as listed
  sn <- vapply(node_vars, function(v) nc_att(nc, v, "standard_name") %||% "", "")
  ix <- which(sn %in% c("longitude", "projection_x_coordinate"))
  iy <- which(sn %in% c("latitude", "projection_y_coordinate"))
  if (length(ix) == 1L && length(iy) == 1L) node_vars <- node_vars[c(ix, iy)]
  node <- data.frame(x = as.numeric(RNetCDF::var.get.nc(nc, node_vars[1L])),
                     y = as.numeric(RNetCDF::var.get.nc(nc, node_vars[2L])))
  node_dim <- a("node_dimension") %||% nc_var_dims(nc, node_vars[1L])[1L]
  nn <- nrow(node)

  td <- as.integer(a("topology_dimension") %||% 2L)
  face_dim <- a("face_dimension")
  edge_dim <- a("edge_dimension")
  start <- NA_integer_
  inferred <- FALSE
  face_node <- NULL
  fnv <- a("face_node_connectivity")
  if (td >= 2L && !is.null(fnv)) {
    m <- read_connectivity(nc, fnv, face_dim, nn)
    face_dim <- attr(m, "entity_dim")
    start <- attr(m, "start_index")
    inferred <- attr(m, "inferred")
    face_node <- face_node_long(m)
  }
  edge <- NULL
  env <- a("edge_node_connectivity")
  if (!is.null(env)) {
    m <- read_connectivity(nc, env, edge_dim, nn)
    edge_dim <- attr(m, "entity_dim")
    if (is.na(start)) start <- attr(m, "start_index")
    edge <- data.frame(node0 = m[, 1L], node1 = m[, 2L])
  }
  UGRID(node, face_node, edge, meta = list(
    mesh = mesh,
    topology_dimension = td,
    dims = c(node = node_dim, edge = edge_dim %||% NA_character_,
             face = face_dim %||% NA_character_),
    start_index = start,
    start_index_inferred = inferred,
    crs = crs_from_nc(nc, node_vars),
    source = normalizePath(dsn)
  ))
}

#' List the arrays in a netCDF file that join to a UGRID model
#'
#' An array joins when one of its dimensions is the model's node, edge or
#' face dimension, matched by name. `dim_map` adds aliases for files that
#' name the dimension differently, such as `c(ncol = "face")`.
#'
#' @param dsn netCDF file (the mesh file itself, or a separate data file).
#' @param x a UGRID model.
#' @param dim_map named character vector, file dimension name to
#'   location ("node", "edge" or "face").
#' @return data.frame: variable, location, the keyed dimension, all
#'   dimensions and shape (both in CDL order, slowest first).
#' @export
ugrid_arrays <- function(dsn, x, dim_map = NULL) {
  key <- x$meta$dims
  key <- stats::setNames(names(key), key)
  key <- key[!is.na(names(key))]
  if (!is.null(dim_map)) key[names(dim_map)] <- dim_map
  nc <- RNetCDF::open.nc(dsn)
  on.exit(RNetCDF::close.nc(nc))
  conn <- unlist(lapply(c("face_node_connectivity", "edge_node_connectivity",
                          "face_edge_connectivity", "face_face_connectivity",
                          "edge_face_connectivity", "node_coordinates",
                          "edge_coordinates", "face_coordinates"),
                        function(n) {
                          v <- nc_att(nc, x$meta$mesh, n)
                          if (is.null(v)) character() else split_words(v)
                        }))
  rows <- lapply(nc_var_names(nc), function(v) {
    if (v %in% conn) return(NULL)
    d <- nc_var_dims(nc, v)
    hit <- d[d %in% names(key)]
    if (length(hit) != 1L) return(NULL)
    len <- vapply(d, function(i) nc_dim_len(nc, i), 1)
    data.frame(variable = v, location = key[[hit]], dim = hit,
               dims = paste(rev(d), collapse = ","),
               shape = paste(rev(len), collapse = "x"))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(variable = character(), location = character(),
                                      dim = character(), dims = character(),
                                      shape = character())
  row.names(out) <- NULL
  out
}

#' Read one array as a keyed array
#'
#' @param dsn netCDF file.
#' @param variable variable name.
#' @param x a UGRID model.
#' @param ... other dimensions to slice, as name = 1-based index, e.g.
#'   `time = 13`. Unsliced dimensions are read in full.
#' @param dim_map as in [ugrid_arrays()].
#' @return a keyed array (the structure of `meshcore::keyed_array()`)
#'   keyed on the mesh dimension, with attribute "location", ready for
#'   `meshcore::join_array()`.
#' @export
ugrid_read_array <- function(dsn, variable, x, ..., dim_map = NULL) {
  arr <- ugrid_arrays(dsn, x, dim_map = dim_map)
  info <- arr[arr$variable == variable, ]
  if (!nrow(info)) stop(variable, " has no node, edge or face dimension of this mesh", call. = FALSE)
  slice <- list(...)
  nc <- RNetCDF::open.nc(dsn)
  on.exit(RNetCDF::close.nc(nc))
  # RNetCDF works in R order (first index fastest) for dims, start, count
  d <- nc_var_dims(nc, variable)
  len <- vapply(d, function(i) nc_dim_len(nc, i), 1)
  start <- rep(1, length(d))
  count <- len
  for (nm in names(slice)) {
    j <- match(nm, d)
    if (is.na(j)) stop("no dimension ", nm, " on ", variable, call. = FALSE)
    start[j] <- slice[[nm]]
    count[j] <- 1
  }
  v <- RNetCDF::var.get.nc(nc, variable, start = start, count = count,
                           collapse = FALSE, unpack = TRUE)
  dims <- stats::setNames(as.numeric(count), d)
  coords <- lapply(stats::setNames(d, d), function(nm) {
    if (nm == info$dim) return(NULL)
    vals <- tryCatch(RNetCDF::var.get.nc(nc, nm, start = start[match(nm, d)],
                                         count = count[match(nm, d)]),
                     error = function(e) NULL)
    vals %||% seq(start[match(nm, d)], length.out = count[match(nm, d)])
  })
  coords <- coords[!vapply(coords, is.null, TRUE)]
  # meshcore's keyed_array structure, built here so reading needs no meshcore
  out <- structure(list(values = array(v, dims), dims = dims, key_dim = info$dim,
                        key = NULL, coords = coords), class = "keyed_array")
  attr(out, "location") <- info$location
  out
}

#' Write a UGRID model (and arrays) to netCDF
#'
#' Writes UGRID 1.0: the mesh topology variable, node coordinates,
#' face_node_connectivity (0-based, fill -1) and edge_node_connectivity.
#' Arrays are keyed arrays from [ugrid_read_array()] with one value per
#' entity (other dimensions sliced), or plain vectors with a "location"
#' attribute.
#'
#' @param x a UGRID model.
#' @param dsn output path.
#' @param arrays named list of arrays.
#' @export
write_ugrid <- function(x, dsn, arrays = list()) {
  check_suggested("RNetCDF")
  m <- x$meta
  mesh <- m$mesh
  dims <- m$dims
  dims[is.na(dims)] <- c(node = "n_node", edge = "n_edge", face = "n_face")[names(dims)[is.na(dims)]]
  nc <- RNetCDF::create.nc(dsn, format = "netcdf4")
  on.exit(RNetCDF::close.nc(nc))
  RNetCDF::dim.def.nc(nc, dims[["node"]], nrow(x$node))
  RNetCDF::dim.def.nc(nc, dims[["edge"]], nrow(x$edge))
  RNetCDF::dim.def.nc(nc, "Two", 2)
  nf <- nrow(x$face)
  if (nf) {
    RNetCDF::dim.def.nc(nc, dims[["face"]], nf)
    RNetCDF::dim.def.nc(nc, "max_face_nodes", max(x$face$n))
  }
  RNetCDF::var.def.nc(nc, mesh, "NC_INT", NA)
  att <- function(v, n, val) RNetCDF::att.put.nc(nc, v, n,
    if (is.character(val)) "NC_CHAR" else if (is.integer(val)) "NC_INT" else "NC_DOUBLE", val)
  att(mesh, "cf_role", "mesh_topology")
  att(mesh, "topology_dimension", as.integer(m$topology_dimension))
  xn <- paste0(mesh, "_node_x"); yn <- paste0(mesh, "_node_y")
  att(mesh, "node_coordinates", paste(xn, yn))
  att(mesh, "node_dimension", dims[["node"]])
  att(mesh, "edge_node_connectivity", paste0(mesh, "_edge_nodes"))
  att(mesh, "edge_dimension", dims[["edge"]])
  if (nf) {
    att(mesh, "face_node_connectivity", paste0(mesh, "_face_nodes"))
    att(mesh, "face_dimension", dims[["face"]])
  }
  lonlat <- identical(m$crs, "OGC:CRS84") || identical(m$crs, "EPSG:4326")
  for (i in 1:2) {
    v <- c(xn, yn)[i]
    RNetCDF::var.def.nc(nc, v, "NC_DOUBLE", dims[["node"]])
    att(v, "standard_name", if (lonlat) c("longitude", "latitude")[i]
        else c("projection_x_coordinate", "projection_y_coordinate")[i])
    att(v, "mesh", mesh); att(v, "location", "node")
    RNetCDF::var.put.nc(nc, v, x$node[[c("x", "y")[i]]])
  }
  if (!is.na(m$crs)) {
    RNetCDF::var.def.nc(nc, "crs", "NC_INT", NA)
    att("crs", "crs_wkt", m$crs)
    att(xn, "grid_mapping", "crs"); att(yn, "grid_mapping", "crs")
  }
  en <- paste0(mesh, "_edge_nodes")
  # RNetCDF takes dimensions in R order: c("Two", n_edge) is CDL (n_edge, Two)
  RNetCDF::var.def.nc(nc, en, "NC_INT", c("Two", dims[["edge"]]))
  att(en, "cf_role", "edge_node_connectivity"); att(en, "start_index", 0L)
  RNetCDF::var.put.nc(nc, en, t(as.matrix(x$edge[c("node0", "node1")])) - 1L)
  if (nf) {
    fv <- paste0(mesh, "_face_nodes")
    RNetCDF::var.def.nc(nc, fv, "NC_INT", c("max_face_nodes", dims[["face"]]))
    att(fv, "cf_role", "face_node_connectivity"); att(fv, "start_index", 0L)
    att(fv, "_FillValue", -1L)
    fm <- face_node_matrix(x) - 1L
    fm[is.na(fm)] <- -1L
    RNetCDF::var.put.nc(nc, fv, t(fm))
  }
  for (nm in names(arrays)) {
    val <- arrays[[nm]]
    loc <- attr(val, "location")
    if (inherits(val, "keyed_array")) val <- val$values
    RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", dims[[loc]])
    att(nm, "mesh", mesh); att(nm, "location", loc)
    RNetCDF::var.put.nc(nc, nm, as.numeric(val))
  }
  att("NC_GLOBAL", "Conventions", "CF-1.8 UGRID-1.0")
  invisible(dsn)
}
