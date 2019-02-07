
tri_ix <- function(x) {
  offset <- rep(seq(0, nrow(x) - 1, by = 3), each = 3)
  c(3L, 1L, 2L) + offset
}

tri_jx <- function(x) {
  offset <- rep(seq(0, nrow(x) - 1, by = 3), each = 3)
  c(1L, 2L, 3L) + offset
}


#' Area of triangles
#'
#' Input is x,y matrix in triplets, with 3 rows per triangle vertex.
#'
#' @param x matrix of triangle coordinates
#'
#' @return numeric, area of triangles
#' @export
#' @examples
#' pts <- structure(c(5L, 3L, 1L, 4L, 4L, 8L, 6L, 9L), .Dim = c(4L, 2L))
#' tri <- c(2, 1, 3, 2, 4, 1)
#' (a <- tri_area(pts[tri, ]))
#' plot(pts)
#' polygon(pts[head(as.vector(rbind(matrix(tri, nrow = 3), NA)), -1), ])
#' text(tapply(pts[tri,1], rep(1:2, each = 3), mean),
#'      tapply(pts[tri,2], rep(1:2, each = 3), mean), labels = sprintf("area: %0.1f", a))
tri_area <- function(x) {
  ix <- tri_ix(x)
  jx <- tri_jx(x)
  abs(colSums(matrix((x[ix, 1] + x[jx, 1]) * (x[ix, 2] - x[jx, 2]), nrow = 3L))/2)
}
