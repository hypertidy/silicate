#' Extract unique vertices
#'
#' @param x model
#' @param ... passed to methods
#' @return data frame of only the unique coordinates
#' @export
#' @examples
#' sc_vertex(minimal_mesh)
#' sc_vertex(SC0(minimal_mesh))
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
# sc_vertex.SC0 <- function(x, ...) {
#   x[["vertex"]] %>% dplyr::mutate(vertex_ = dplyr::row_number())
# }
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
sc_vertex.TRI0 <- function(x, ...) {
  x[["vertex"]]
}

#' @name sc_vertex
#' @export
sc_vertex.PATH <- function(x, ...) {
  x[["vertex"]]
}
#' @name sc_vertex
#' @export
sc_vertex.PATH0 <- function(x, ...) {
  x[["vertex"]]
}
