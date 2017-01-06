
## see st_cast and c.sfg
Paste0 <- function (lst)  lapply(lst, unclass)
# path of indexes to pairs in a matrix
path_to_seg <- function (x) {
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2,
                               byrow = FALSE)), -2L)
}
## worker for sf vector/matrix coords
## XY only
m_or_v_XY <- function(x) {
  x <- unclass(x)
  if (is.null(dim(x))) x <- matrix(x, nrow = 1L)
  x[, 1:2, drop = FALSE]
}
# turn sf coords into a data frame
df_data <- function(x) setNames(as.data.frame(m_or_v_XY(x)), c("x", "y"))
## convert everything to a flat list of data frames
## (hierarchy matches sp, everything is a POLYGON/MULTILINESTRING)
paths_as_df <- function(x) {
  ##if (is_POINT or MULTIPOINT or GC)  TODO
  ## this will probably error unless you are using dev from Github (2017-01-06)
  x <- st_cast(x, "MULTILINESTRING")
  rapply(unclass(x), f = df_data,
         classes = c("numeric", "matrix"), how = "list")
}
objects_as_df <- function(x) {
  dat <- dplyr::bind_rows(lapply(st_geometry(x), function(a) dplyr::bind_rows(paths_as_df(a), .id = "branch_")), .id = "object_")
  dat$piece <- factor(dat$branch_)
  dat$branch_ <- factor(paste0(dat$object_, dat$branch_, sep = "-"))
  dat
}
dp_triangulate_sf <- function(x, ...) {
  coords <- objects_as_df(x)
  coords[["vertex_"]] <- as.integer(factor(paste(coords[["x"]], coords[["y"]], sep = "-")))
  b_link_v <- coords[, c("branch_", "vertex_")]
  vertices <- coords[!duplicated(b_link_v[["vertex_"]]), c("x", "y", "vertex_")]
  vertices <- vertices[order(vertices[["vertex_"]]), ]

  segments <- do.call(rbind, lapply(split(b_link_v[["vertex_"]], b_link_v[["branch_"]]),
                                    function(x) path_to_seg(x))
  )
  ## do we need to remove duplicated segments??
  bad <- duplicated(cbind(pmin(segments[, 1], segments[, 2]), pmax(segments[, 1], segments[, 2])))
  ## this is slow
  #bad <- duplicated(t(apply(segments, 1, sort)))
  ps <- RTriangle::pslg(P = as.matrix(vertices[, c("x", "y")]), S = segments[!bad, ])
  tr <- RTriangle::triangulate(ps, ...)
  # this is slow
  #g <- st_sfc(lapply(split(as.vector(t(tr$T)), rep(seq_len(nrow(tr$T)), each = 3)),
   #                  function(x) st_polygon(list(tr$P[c(x, x[1L]), ]))), crs = st_crs(x))
  ## this is FAST
  g <- st_sfc(lapply(split(as.vector(t(tr$T)), rep(seq_len(nrow(tr$T)), each = 3)),
                     function(x) structure(list(tr$P[c(x, x[1L]), ]), class = c("XY", "POLYGON", "sfg"))), crs = st_crs(x))
  ## now intersect triangle centroids with original layer to drop holes
  ## any bbox optims for sf yet?, needs to be like sp::over()
  ov <- sp::over(as(st_centroid(g), "Spatial"), as(as(x, "Spatial"), "SpatialPolygons"))
  drop <- is.na(ov)
  g <- g[!drop]
  ov <- ov[!drop]
  feature_triangle <- data.frame(triangle = seq_along(g), object_ = factor(ov))
  feature_triangle[["geometry"]] <- g[feature_triangle$triangle]
  st_as_sf(feature_triangle)
}
