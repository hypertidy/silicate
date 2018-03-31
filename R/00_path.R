
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
sc_path <- function(x, ...) {
  UseMethod("sc_path")
}
#' @name sc_path
#' @export
sc_path.default <- function(x, ...) {
  PATH(x)[["path"]]
}
#' @name sc_path
#' @export
sc_path.PATH <- function(x, ...) {
  x[["path"]]
}
#' @name sc_path
#' @export
sc_path.ARC <- function(x, ...) {
  stop("sc_path not yet supported for ARC")
}
#' @name sc_path
#' @export
sc_path.SC <- function(x, ...) {
  stop("sc_path not yet supported for SC")
}
xypaths <- function(x) {
  g <- cumsum(c(0, abs(diff(is.na(x[[1]])))))[!is.na(x[[1]])]
  as.integer(table(g))
}
#' @name sc_path
#' @export
sc_path.default <- function(x, ...) {
  if (is_r_coords(x)) {
    return(tibble::tibble(nrow = xypaths(x)))
  }
  tibble::tibble(nrow = length(x[[1L]]))
}



## --------------------------------------------------------
## sf
## --------------------------------------------------------



#' @importFrom purrr map_df
sc_atom <- function(x, ...) faster_as_tibble(list(ncoords_= nrow(x),
                                                  path_ = sc_uid(1L)))
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
  x[["object_"]] <- ids[x[["object"]]]
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}

gibble_path <- function(x,  ...) {
  out <- gibble::gibble(x)
  out[["path_"]] <- sc_uid(nrow(out))
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





## --------------------------------------------------------
## sp
## --------------------------------------------------------



#' @importFrom methods slotNames
.sp_get_geometry <- function(x) {
  slt <- slotNames(x)
  if ("polygons" %in% slt) {
    return(slot(x, "polygons"))
  }
  if ("lines" %in% slt) {
    return(slot(x, "lines"))
  }
  out <- slot(x, "coords")
  if (!is.recursive(out)) out <- list(out)
  out
}
#' @name sc_path
#' @importFrom stats setNames
#' @export
sc_path.Spatial <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x)
  if (is.null(ids)) {
    ids <- sc_uid(length(unique(x[["object"]])))
  }
  x[["object_"]] <- ids[x[["object"]]]
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}


## --------------------------------------------------------
## trip
## --------------------------------------------------------


#' @export
sc_path.trip <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x) %>%
    dplyr::mutate(object_ = .data$object)
  if (is.null(ids)) {
    ids <- sc_uid(length(unique(x[["object_"]])))
  }
  x[["object_"]] <- ids[x[["object_"]]]
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}


## --------------------------------------------------------
## ade
## --------------------------------------------------------


#' @export
sc_path.ltraj <- function(x, ...) {
  ## gibble not really needed
  out <- tibble::as_tibble(cbind(nrow = unlist(lapply(x, nrow)), ncol = 3L))
  out[["type"]] <- "ltraj"
  out
}
