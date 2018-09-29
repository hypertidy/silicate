## MDSumner 2018-09-29
## bash at a vector type with a shared vertex pool


# library(silicate)
# sc <- silicate::TRI(minimal_mesh)
# x1 <- c_MESH(lapply(split(tibble::as_tibble(matrix(match(as.matrix(sc$triangle[1:3]), sc$vertex$vertex_), ncol  = 3)), 1:nrow(sc$triangle)), sc_triangle), vertex_pool = sc$vertex[1:2])
# x2 <- c_MESH(sc_triangle(tibble::tibble(V1 = 6, V2 = 7, V3 = 14)),
#        sc_triangle(tibble::tibble(V1 = 14, V2 = 13, V3 = 6)), vertex_pool = sc$vertex[1:2])
#
# plot(x1)
# plot(x2, add = TRUE, border = "blue")
#


## atomic index element
sc_triangle <- function(x) {
  structure(tibble::as_tibble(x), class = c("MESH", "tbl_df", "data.frame"))
}

## vector of atomic index elements
c_MESH <- function(..., vertex_pool = NULL) {
  UseMethod("c_MESH")
}
c_MESH.default <- function(..., vertex_pool = NULL) {
  stopifnot(is.data.frame(vertex_pool))
  structure(list(...), class = c("cMESH"), vertex_pool = vertex_pool)
}
c_MESH.list <- function(..., vertex_pool = NULL) {
  structure(list(...)[[1]], class = c("cMESH"), vertex_pool = vertex_pool)
}
vpool <- function(x) attr(x, "vertex_pool")
print.cMESH <- function(x, ...) {
  cat(sprintf("object of class: %s\n", class(x)[1]))
  cat(sprintf("     primitives: %i\n", length(x)))
  cat(sprintf("       vertices: %i\n", nrow(vpool(x))))

}
plot.cMESH <- function(x, ..., add = FALSE) {
  pool <- vpool(x)
  if (!add) plot(pool, type = "n")
  polygon(pool[do.call(rbind, lapply(x, function(a) t(cbind(as.matrix(a)[, c(1, 2, 3, 1), drop = FALSE], NA)))), ], ...)
   invisible(NULL)
}



