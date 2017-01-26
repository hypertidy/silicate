sf_to_gris_names <- 
  #names(gtab) <- sf_to_gris_names(names(gtab))
  
  m_v <- function(x) {
    x <- unclass(x)
    x <- if (is.null(dim(x))) t(x) else x
    x
  }
geometry_dimension <- function(x) rev(class(x))[3L]
sf_geom_names <- function(x) unlist(strsplit(geometry_dimension(x), ""))
sc_geom_names <- function(gnames) {
  gnames <- gsub("^X$", "x_", gnames)
  gnames <- gsub("^Y$", "y_", gnames)
  gnames <- gsub("^Z$", "z_", gnames)
  gnames <- gsub("^M$", "m_", gnames)
  gnames <- gsub("^type$", "type_", gnames)
  gnames
}
sfcoords <- function(x, ...) tibble::as_tibble(m_v(x))

#' Coordinate decomposition
#' 
#' Collect all coordinates in one table, the path_link_vertex table
#' has the information about the original grouping.  
#' 
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_coord
#' @export
#' @seealso `sc_path` for the central part of the model, `sc_object` for 
#' the features, and `PATH` for the full model. 
#' @examples 
#' data("sfzoo")
#' lapply(sfzoo, sc_coord)
sc_coord <- function(x, ...) UseMethod("sc_coord")
#' @importFrom sf st_geometry
#' @name sc_coord
#' @export
#' @examples
#' sc_coord(sf::st_sfc(sfzoo))
sc_coord.sf <- function(x, ...) {
  sc_coord(sf::st_geometry(x), ...)
}
#' @importFrom dplyr bind_rows
#' @name sc_coord
#' @export
#' @examples 
#' sc_coord(sf::st_sfc(sfzoo))
sc_coord.sfc <- function(x,  ...) {
  x <- lapply(x, sc_coord)
  dplyr::bind_rows(x)
}
#' @name sc_coord
#' @export
#' @importFrom  stats setNames 
sc_coord.MULTIPOLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x))
  setNames(dplyr::bind_rows(lapply(x, function(y) dplyr::bind_rows(lapply(y, sfcoords)))), colnames)
}
#' @name sc_coord
#' @export
sc_coord.POLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x))
  setNames(dplyr::bind_rows(lapply(x, sfcoords)), colnames)
}
#' @name sc_coord
#' @export
sc_coord.MULTILINESTRING <- sc_coord.POLYGON
#' @name sc_coord
#' @export
sc_coord.LINESTRING <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x))
  setNames(sfcoords(x), colnames)
}
#' @name sc_coord
#' @export
sc_coord.MULTIPOINT <- sc_coord.LINESTRING
#' @name sc_coord
#' @export
sc_coord.POINT <- sc_coord.LINESTRING
