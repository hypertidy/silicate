#' @export
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
#' @export
sc_vertex.default <- function(x, ...) {
  PATH(x)[["vertex"]]
}
#' @export
sc_vertex.SC <- function(x, ...) {
  x[["vertex"]]
}