
#' @importFrom ids random_id
sc_rand <- function(n = 1L) ids::random_id(n, bytes = 4)
#' @importFrom tibble tibble
sc_atom <- function(x, ...) tibble::tibble(ncoords_= nrow(x), path_ = sc_rand())
sc_list <- function(x) dplyr::bind_rows(lapply(x, sc_atom))

#' Path decomposition
#' 
#' Start in the middle, and build the 'path-link-vertex' table. 
#'
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_path
#' @export
#' @seealso `sc_coord` for the coordinates part of the model, `sc_object` for 
#' the features, and `PATH` for the full model. 
#' @examples 
#' data("sfzoo")
#' lapply(sfzoo, sc_path)
sc_path <- function(x, ...) {
  UseMethod("sc_path")
}

#' @importFrom sf st_geometry
#' @name sc_path
#' @export
#' @examples
#' sc_path(sf::st_sfc(sfzoo))
sc_path.sf <- function(x, ...) {
  sc_path(sf::st_geometry(x), ...)
}

#' @param ids object id, one for each object in the `sfc`
#'
#' @importFrom dplyr bind_rows
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sf::st_sfc(sfzoo))
sc_path.sfc <- function(x, ids = NULL, ...) {
  x <- lapply(x, sc_path)
  if (!is.null(ids)) {
    stopifnot(length(ids) == length(x))
    x <- lapply(seq_along(x), function(a) dplyr::bind_cols(x[[a]], tibble::tibble(object_ = rep(ids[a], nrow(x[[a]])))))
  }
  dplyr::bind_rows(x)
}
#' @name sc_path
#' @export
#' @examples 
#' 
#' sc_path(sfzoo$multipolygon)
sc_path.MULTIPOLYGON <- function(x, ...) {
  dplyr::bind_rows(lapply(x, sc_list), .id = "island_")
}
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$polygon)
sc_path.POLYGON <- function(x, ...) {
  sc_list(x)
}
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$linestring)
sc_path.LINESTRING <- sc_atom
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multilinestring)
sc_path.MULTILINESTRING <- sc_path.POLYGON
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$point)
sc_path.POINT <- function(x, ...) sc_atom(matrix(x, nrow = 1L))
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multipoint)
sc_path.MULTIPOINT <- function(x, ...) tibble::tibble(ncoords_ = 1, path_ = sc_rand(n = nrow(x)))
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multipoint)
sc_path.GEOMETRYCOLLECTION <- function(x, ...) dplyr::bind_rows(lapply(x, sc_path), .id = "collection_")


## infix sugar for if (is.null)
"%||%" <- function(a, b) {
  if (is.null(a)) b else a
}
