#' Objects, features
#'
#' The objects are the front end entities, the usual "GIS contract" objects,
#' or features.
#'
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_object
#' @return data frame of the object values
#' @export
#' @seealso `sc_coord` for the coordinates part of the model, `sc_path` for
#' the central part of the model, and `PATH` for the full model.
#' @examples
#' sc_object(minimal_mesh)
#' sc_object(SC0(minimal_mesh))
sc_object <- function(x, ...) UseMethod("sc_object")

#' @name sc_object
#' @export
sc_object.default <- function(x, ...) {
  as_tibble(x[["object"]])
}



## --------------------------------------------------------
## sf
## --------------------------------------------------------


## a function sf should have
## to drop the spatial stuff
.st_set_geometry <- function(x, value = NULL) {
  #st_geometry(x) <- value
  x[[attr(x, "sf_column")]] <- NULL
  as.data.frame(x)
}

.st_get_geometry <- function(x) {
  x[[attr(x, "sf_column")]]
}

#' Objects, features
#'
#' The objects are the front end entities, the usual "GIS contract" objects,
#' the features.
#'
#' @seealso `sc_coord` for the coordinates part of the model, `sc_path` for
#' the central part of the model, and `PATH` for the full model.
#' @name sc_object
#' @importFrom tibble as_tibble
#' @export
sc_object.sf <- function(x, ...) {
  tibble::as_tibble(.st_set_geometry(x))
}

#' @name sc_object
#' @export
sc_object.sfc <- function(x, ...) {
  tibble(object_ = sc_uid(length(x)))
}


## --------------------------------------------------------
## sp
## --------------------------------------------------------

#' @export
#' @importFrom methods .hasSlot slot
sc_object.Spatial <- function(x, ...) {

  if (!.hasSlot(x, "data")) {
    out <- setNames(list(seq_along(x)), class(x))
  } else {
    out <- methods::slot(x, "data")
  }
  tibble::as_tibble(out)
}

## --------------------------------------------------------
## trip
## --------------------------------------------------------



#' @export
sc_object.trip <- function(x, ...) {
  tor <- slot(x, "TOR.columns")
  tibble::tibble(trip = unique(x[[tor[2L]]]))
}
