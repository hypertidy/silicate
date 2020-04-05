
#' Path decomposition
#'
#' Start in the middle, and build the 'path-link-vertex' table.
#'
#' Paths have properties of their type, their number of vertices, their geometric
#' dimension and which object they occur in.
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @name sc_path
#' @export
#' @return data frame of path identity and properties
#' @seealso `sc_coord` for the coordinates part of the model, `sc_object` for
#' the features, and `PATH` for the full model.
#' @examples
#' sc_path(minimal_mesh)
#' sc_path(PATH(minimal_mesh))
sc_path <- function(x, ...) {
  UseMethod("sc_path")
}

#' @name sc_path
#' @export
sc_path.list <- function(x, ids = NULL, ...) {
  ## check we aren't an unclass sfc list
  if (inherits(x[[1]], "sfg")) {
    return(sc_path.sfc(x, ids = ids))
  } else {
    out <- try(sc_path.default(x), silent = TRUE)
    if (inherits(out, "try-error")) stop("cannot determine coords from 'x'")
  }
  out
}
#' @name sc_path
#' @export
sc_path.default <- function(x, ...) {
  if (is_r_coords(x)) {
    return(tibble::tibble(nrow = xypaths(x)))
  }
  sc_path(PATH0(x))
}
#' @name sc_path
#' @export
sc_path.PATH <- function(x, ...) {
  x[["path"]]
}
#' @name sc_path
#' @export
sc_path.PATH0 <- function(x, ...) {
  do.call(rbind, x$object$path_) %>%
    dplyr::group_by(.data$object_, .data$path_, .data$subobject) %>%
    dplyr::summarize(ncoords_ = dplyr::n()) %>% ungroup()
}
#' @name sc_path
#' @export
sc_path.ARC <- function(x, ...) {
  ## arcs are paths from this perspective,
  ## and are each an object (so LINESTRING)
  arcs <- x$arc_link_vertex %>%
    dplyr::group_by(.data$arc_) %>% dplyr::tally()
  #arcs <- arcs[match(arcs$arc_, unique(x$object_link_arc$arc_)), ]\

  tibble(ncol = 2L, type = "LINESTRING",
         subobject = 1L, object_ = arcs$arc_,
         #object_ = x$object_link_arc$object_[match(arcs$arc_, x$object_link_arc$arc_)],
         path_ = arcs$arc_, ncoords_ = arcs$n)
}
#' @name sc_path
#' @export
sc_path.SC <- function(x, ...) {
  stop("sc_path not yet supported for SC")
}
#' @name sc_path
#' @export
sc_path.SC0 <- function(x, ...) {
  stop("sc_path not yet supported for SC")
}

xypaths <- function(x) {
  g <- cumsum(c(0, abs(diff(is.na(x[[1]])))))[!is.na(x[[1]])]
  as.integer(table(g))
}
#' @name sc_path
#' @export
sc_path.matrix <- function(x, ...) {
  sc_path(tibble::as.tibble(x))
}

## --------------------------------------------------------
## sf
## --------------------------------------------------------






#' Common path forms.
#'
#' Paths.
#'
#' @name sc_path
#' @export
#' @export sc_path
sc_path.sf <- function(x, ids = NULL, ...) {
  sc_path(.st_get_geometry(x), ids = ids, ...)
}

#' @param ids object id, one for each object in the `sfc`
#'
#' @importFrom dplyr bind_rows
#' @name sc_path
#' @export
sc_path.sfc <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x)
  if (is.null(ids)) {
    ids <- sc_uid(length(unique(x[["object"]])))
  }
  x[["object_"]] <- ids[x[["object"]]]
  if (is.null(x[["subobject"]])) x[["subobject"]] <- 1
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  #x[["suboject"]] <- NULL
  x[["object"]] <- NULL
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
