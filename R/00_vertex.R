#' Extract unique vertices
#'
#' @param x model
#' @param ... passed to methods
#'
#' @export
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
#' @export
sc_vertex.default <- function(x, ...) {
  SC(x)[["vertex"]]
}
#' @export
sc_vertex.SC <- function(x, ...) {
  x[["vertex"]]
}
#' @export
sc_vertex.ARC <- function(x, ...) {
  x[["vertex"]]
}
#' @export
sc_vertex.PATH <- function(x, ...) {
  x[["vertex"]]
}