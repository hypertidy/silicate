
#' @importFrom purrr map_df
sc_atom <- function(x, ...) faster_as_tibble(list(ncoords_= nrow(x), path = sc_uid()))
sc_list <- function(x) {
  dplyr::bind_rows(lapply(x, sc_atom))
}
#sc_atom <- function(x, ...) UseMethod("sc_atom")
#sc_atom.matrix <- function(x, ...) cbind(nrow(x), ncol(x))
#sc_atom.list <- function(x, ...) lapply(x, sc_atom)
#sc_atom_mat <- function(x, ...) nrow(x)
#sc_list_mat <- function(x) unlist(lapply(x, sc_atom_mat))

#' Common path forms. 
#' 
#' Paths. 
#' 
#' @name sc_path
#' @export
#' @export sc_path
#' @examples
#' #library(scsf)
#' #sf_dataset <- sf::st_sf(geometry = sf::st_sfc(sfzoo[[2]]), a = 1)
#' #PATH(sf_dataset)
#' #sc_path(sf::st_sfc(sfzoo))
sc_path.sf <- function(x, ids = NULL, ...) {
  sc_path(.st_get_geometry(x), ids = ids, ...)
}

#' @param ids object id, one for each object in the `sfc`
#'
#' @importFrom dplyr bind_rows
#' @name sc_path
#' @export
#' @examples 
#' #sc_path(sf::st_sfc(sfzoo))
sc_path.sfc <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x)
  if (is.null(ids)) {
     ids <- sc_uid(length(unique(x[["object"]])))
   } 
   x[["object"]] <- ids[x[["object"]]]
   x[["path"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}

gibble_path <- function(x,  ...) {
  out <- gibble::gibble(x)
  out[["path"]] <- sc_uid(nrow(out))
  out
}

#' @name sc_path
#' @export
#' @importFrom dplyr bind_rows mutate row_number
#' @examples
#' sc_path(sfzoo$multipolygon)
sc_path.MULTIPOLYGON <- function(x, ...) {
  gibble_path(x)
}

#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$polygon)
sc_path.POLYGON <- function(x, ...) {
  gibble_path(x)
}
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$linestring)
sc_path.LINESTRING <- function(x, ...) gibble_path(x)
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multilinestring)
sc_path.MULTILINESTRING <- function(x, ...) gibble_path(x)
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$point)
sc_path.POINT <- function(x, ...) gibble_path(x)
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multipoint)
sc_path.MULTIPOINT <- function(x, ...) gibble_path(x)
#' @name sc_path
#' @export
#' @examples 
#' sc_path(sfzoo$multipoint)
sc_path.GEOMETRYCOLLECTION <- function(x, ...) lapply(x, gibble_path)



