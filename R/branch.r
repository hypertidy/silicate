
#' @importFrom ids random_id
sc_rand <- function(n = 1L) ids::random_id(n, bytes = 8)
#' @importFrom tibble tibble
sc_atom <- function(x, ...) tibble::tibble(ncoords_= nrow(x), branch_ = sc_rand())
sc_list <- function(x) dplyr::bind_rows(lapply(x, sc_atom))

#' Branch decomposition
#' 
#' Start in the middle, and build the 'branch-link-vertex' table. 
#' @name sc_branch
#' @export
#' @seealso `sc_coord` for the coordinates part of the model, `sc_object` for 
#' the features, and `BRANCH` for the full model. 
#' @examples 
#' zoo <- sfzoo()
#' lapply(zoo, sc_branch)
sc_branch <- function(x, ...) UseMethod("sc_branch")

#' @importFrom sf st_geometry
#' @name sc_branch
#' @export
#' @examples
#' sc_branch(sfzoo())
sc_branch.sf <- function(x, ...) {
  sc_branch(sf::st_geometry(x), ...)
}
#' @importFrom dplyr bind_rows
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(st_sfc(zoo))
sc_branch.sfc <- function(x, ids = NULL, ...) {
  x <- lapply(x, sc_branch)
  if (!is.null(ids)) {
    stopifnot(length(ids) == length(x))
    x <- lapply(seq_along(x), function(a) dplyr::bind_cols(x[[a]], tibble::tibble(object_ = rep(ids[a], nrow(x[[a]])))))
  }
  dplyr::bind_rows(x)
}
#' @name sc_branch
#' @export
#' @examples 
#' 
#' sc_branch(zoo$multipolygon)
sc_branch.MULTIPOLYGON <- function(x, ...) {
  dplyr::bind_rows(lapply(x, sc_list), .id = "island_")
}
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$polygon)
sc_branch.POLYGON <- function(x, ...) {
  sc_list(x)
}
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$linestring)
sc_branch.LINESTRING <- sc_atom
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$multilinestring)
sc_branch.MULTILINESTRING <- sc_branch.POLYGON
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$point)
sc_branch.POINT <- function(x, ...) sc_atom(matrix(x, nrow = 1L))
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$multipoint)
sc_branch.MULTIPOINT <- function(x, ...) tibble::tibble(ncoords_ = 1, branch_ = sc_rand(n = nrow(x)))
#' @name sc_branch
#' @export
#' @examples 
#' sc_branch(zoo$multipoint)
sc_branch.GEOMETRYCOLLECTION <- function(x, ...) dplyr::bind_rows(lapply(x, sc_branch), .id = "collection_")


## infix sugar for if (is.null)
"%||%" <- function(a, b) {
  if (is.null(a)) b else a
}
