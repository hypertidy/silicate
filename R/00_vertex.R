#' Extract unique vertices
#'
#' @param x model
#' @param ... passed to methods
#'
#' @export
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
#' @name sc_vertex
#' @export
sc_vertex.default <- function(x, ...) {
  SC(x)[["vertex"]]
}
#' @name sc_vertex
#' @export
sc_vertex.SC <- function(x, ...) {
  x[["vertex"]]
}
#' @name sc_vertex
#' @export
sc_vertex.ARC <- function(x, ...) {
  x[["vertex"]]
}
#' @name sc_vertex
#' @export
sc_vertex.TRI <- function(x, ...) {
  x[["vertex"]]
}
#' @name sc_vertex
#' @export
sc_vertex.PATH <- function(x, ...) {
  x[["vertex"]]
}