#' object-value for cell-based raster (discrete pixels)
#' (continuous pixels store values with coordinates)
sc_object.RasterLayer <- function(x, ...) {
  tibble::as_tibble(stats::setNames(list(seq_len(x@nrows * x@ncols), values(x)), c("object_", names(x))))
}


#' corner coordinates
sc_coord.RasterLayer <- function(x, ...) {
  setNames(tibble::as_tibble(quadmesh:::edgesXY(x)), c("x_", "y_"))
}


sc_path.RasterLayer <- function(x, ...) {
  tibble::tibble(ncoords_ = 4, path_ = sc_uid(x@nrows * x@ncols))
}



#' the topology
#'
#'

## from quadmesh:::prs
pairs_index <- function (x) {
  cbind(head(x, -1), tail(x, -1))
}
## from quadmesh:::p4
pair_four <- function (xp, nc)
{
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}

## mesh from quadmesh::quadmesh
mesh_index <- function(x, ...) {
  x <- x[[1]]
  #exy <- sc_coord(x)
  ind <- apply(pairs_index(seq(ncol(x) + 1)), 1, pair_four, nc = ncol(x) + 1)
  ind0 <- as.vector(ind) + rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  ind1 <- t(matrix(ind0, nrow = 4))
  tibble::as_tibble(ind1)
}