#sf_to_gris_names <- 
  #names(gtab) <- sf_to_gris_names(names(gtab))
  
  m_v <- function(x) {
    x <- unclass(x)
    x <- if (is.null(dim(x))) t(x) else x
    x
  }
geometry_dimension <- function(x) {
  
  out <- rev(class(x))[3L]
  ## catch for https://github.com/hypertidy/silicate/issues/59#issuecomment-371023216
  if (out == "list") out <- "XY" #:|
  out
}
sf_geom_names <- function(x) unlist(strsplit(geometry_dimension(x), ""))
sc_geom_names <- function(gnames) {
  gnames <- gsub("^X$", "x_", gnames)
  gnames <- gsub("^Y$", "y_", gnames)
  gnames <- gsub("^Z$", "z_", gnames)
  gnames <- gsub("^M$", "m_", gnames)
  gnames <- gsub("^type$", "type_", gnames)
  gnames
}
sfcoords <- function(x, ...) faster_as_tibble(m_v(x))


#' Coordinate decomposition
#' 
#' Collect all coordinates in one table, the path_link_vertex table
#' has the information about the original grouping.  
#' 
#' @seealso `sc_path` for the central part of the model, `sc_object` for 
#' the features, and `PATH` for the full model. 
#' @name sc_coord
#' @export
#' @examples
#' data("sfzoo")
#' #sc_coord(sf::st_sfc(sfzoo))
#'# lapply(sfzoo, sc_coord)
sc_coord.sf <- function(x, ...) {
  sc_coord(.st_get_geometry(x), ...)
}
#' @importFrom dplyr bind_rows
#' @name sc_coord
#' @export
#' @examples 
#' #sc_coord(sf::st_sfc(sfzoo))
sc_coord.sfc <- function(x,  ...) {
  x <- lapply(x, sc_coord)
  dplyr::bind_rows(x)
  
}


# these are short-cut methods for single-type sets
#' @export
sc_coord.sfc_MULTIPOLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x[[1]]))
  out <- tibble::as_tibble(do.call(rbind, lapply(x, function(y) do.call(rbind, lapply(unclass(y), function(a) do.call(rbind, a))))))
  setNames(out, colnames)
           
}
#' @export
sc_coord.sfc_MULTILINESTRING <- sc_coord.sfc_POLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x[[1]]))
  out <- tibble::as_tibble(do.call(rbind, lapply(x, function(y) do.call(rbind, unclass(y)))))
  setNames(out, colnames)
  
}
#' @export
sc_coord.sfc_LINESTRING <- sc_coord.sfc_MULTIPOINT <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x[[1]]))
  out <- tibble::as_tibble(do.call(rbind, unclass(x)))
  setNames(out, colnames)
}
#' @export
sc_coord.sfc_POINT <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x[[1]]))
  out <- tibble::as_tibble(do.call(rbind, unclass(x)))
  setNames(out, colnames)
}

#' @name sc_coord
#' @export
#' @importFrom  stats setNames 
sc_coord.MULTIPOLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x))
  setNames(dplyr::bind_rows(lapply(x, function(y) dplyr::bind_rows(lapply(y, sfcoords)))), colnames)
  #setNames(purrr::map_df(x, function(y) purrr::map_df(y, sfcoords)), colnames)
}
#' @name sc_coord
#' @export
sc_coord.POLYGON <- function(x, ...) {
  colnames <- sc_geom_names(sf_geom_names(x))
  setNames(dplyr::bind_rows(lapply(x, sfcoords)), colnames)
 # setNames(purrr:map_df(x, sfcoords), colnames)
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
