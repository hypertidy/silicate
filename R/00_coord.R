#' Coordinate decomposition
#'
#' Collect all instances of all coordinates in one table. This complementary to the `sc_path` of
#' an object, since the number of coordinates per path gives a structural mapping into this set.
#'
#' @param x input model
#' @param ... arguments passed to methods
#'
#' @name sc_coord
#' @return data frame of all the coordinates in the order they occur
#' @export
#' @seealso `sc_path` for the central part of the model, `sc_object` for
#' the features, and `PATH` for the full model.
#' @examples
#' sc_coord(minimal_mesh)
#' sc_coord(SC(minimal_mesh))
sc_coord <- function(x, ...) UseMethod("sc_coord")

## --------------------------------------------------------
## generic data - lists and data frames, xy.coords, xyz.coords and other
## --------------------------------------------------------

## a list
## a matrix
## a data frame
is_r_coords <- function(x) {
  if (!inherits(x, "data.frame")) {
    l <- lengths(x)
    ## short-circuit, we aren't equal lengths
    if (length(unique(l)) > 1L) return(FALSE)
  }
  if (anyNA(x[[1]])) {
    nas <- lapply(x, function(a) which(is.na(a)))
    return(all(unique(unlist(nas)) ==  nas[[1]]))
  }
  FALSE
}
r_coords <- function(x) {
  nas <- is.na(x[[1]])
  tibble::as_tibble(x)[!nas, ]
}

## -- from anglr, suited for a data frame
maybe_geom_column <- function(x, ...) {
  names(x)[purrr::map_lgl(x, ~ inherits(.x, "list"))]
}

check_is_geom_column <- function(x, ...) {
  any(class(x[[1]]) == "sfg")
}

#' @name sc_coord
#' @export
sc_coord.list <- function(x, ...) {
  ## before we bail out to sc_coord.default, check if this is an unclass sfc
  if (inherits(x[[1]], "sfg")) {
    ## assume it is a sfc
    return(sc_coord.sfc(x))
  } else {
    out <- try(sc_coord.default(x), silent = TRUE)
  }
  if (inherits(out, "try-error")) {
    stop("cannot interpret coords from 'x'")
  }
  out
}
#' @name sc_coord
#' @export
sc_coord.default <- function(x, ...){
  if (inherits(x, "Spatial")) return(sc_coord.Spatial(x))
  if (is.null(x[["coord"]]) || !inherits(x[["coord"]], "data.frame")) {
    if (is.list(x)) x <- tibble::as_tibble(x)
    geo <- maybe_geom_column(x)
    if (length(geo) > 0) {
      if (check_is_geom_column(x[[geo]])) {
      return(sc_coord(x[[geo]]))
      } else {
        stop("cannot find coords")
      }
    }
    ## we might xy.coords
    if (is_r_coords(x)) x <- r_coords(x)
  } else {
    x <- tibble::as_tibble(x[["coord"]])
  }

  x
}


#' @name sc_coord
#' @export
sc_coord.matrix <- function(x, ...){
  sc_coord(tibble::as_tibble(x))
}


#' @name sc_coord
#' @export
sc_coord.ARC <- function(x, ...) {
  x$arc_link_vertex %>%
    dplyr::inner_join(sc_vertex(x), "vertex_") %>%
    dplyr::select(-.data$arc_, -.data$vertex_)
}
#' @name sc_coord
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join transmute_ select_
sc_coord.PATH <- function(x, ...) {
  dplyr::inner_join(dplyr::transmute(x[["path_link_vertex"]], vertex_ = .data$vertex_) ,
                    x[["vertex"]], "vertex_") %>%
    dplyr::select(-.data$vertex_)
}
#' @name sc_coord
#' @export
sc_coord.TRI <- function(x, ...) {
  sc_coord(SC(x))
}
#' @name sc_coord
#' @export
sc_coord.PATH0 <- function(x, ...) {
  x[["vertex"]][do.call(rbind, x$object$path_)[["vertex_"]], ]
}
#' @name sc_coord
#' @export
sc_coord.SC0 <- function(x, ...) {
  id <- do.call(rbind, x$object[["topology_"]])[c(".vx0", ".vx1")]

  x[["vertex"]][as.vector(t(as.matrix(id))), ]
}
#' @name sc_coord
#' @export
sc_coord.SC <- function(x, ...) {
  sc_coord(SC0(x), ...)
}

#' Coordinate decomposition
#'
#' Collect all coordinates in one table, the path_link_vertex table
#' has the information about the original grouping.
#'
#' @seealso `sc_path` for the central part of the model, `sc_object` for
#' the features, and `PATH` for the full model.
#' @name sc_coord
#' @export
sc_coord.sf <- function(x, ...) {
  sc_coord(.st_get_geometry(x), ...)
}
#' @importFrom dplyr bind_rows
#' @name sc_coord
#' @export
sc_coord.sfc <- function(x,  ...) {
  #do.call(rbind, lapply(x, sc_coord))
  dplyr::bind_rows(lapply(x, sc_coord))

}


#' @name sc_coord
#' @export
sc_coord.pslg <- function(x, ...) {
  ## assume segments are of interest
  m <- x$P[t(x$S), ]
  tibble::tibble(x_ = m[,1L], y_ = m[,2L])
}


## --------------------------------------------------------
## sf
## --------------------------------------------------------

m_v <- function(x) {
  x <- unclass(x)
  x <- if (is.null(dim(x))) t(x) else x
  x
}
geometry_dimension <- function(x) {

  out <- rev(class(x))[3L]
  ## catch for https://github.com/hypertidy/silicate/issues/59#issuecomment-371023216
  ## remove this hack when https://github.com/r-spatial/sf/issues/660 released
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
sfcoords <- function(x, ...) as.data.frame(m_v(x))



split_mat <- function(x) setNames(split(x, rep(seq_len(dim(x)[2]),
                                               each = dim(x)[1])), c("x_", "y_"))


# these are short-cut methods for single-type sets
#' @export
sc_coord.sfc_MULTIPOLYGON <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x[[1]]))
  mat <- do.call(rbind, lapply(x, function(y) do.call(rbind, lapply(unclass(y), function(a) do.call(rbind, a)))))
  colnames(mat) <- colnames0
  sm <- split_mat(mat)
  tibble::new_tibble(sm, nrow = length(sm[[1L]]))

}
#' @export
sc_coord.sfc_MULTILINESTRING <- sc_coord.sfc_POLYGON <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x[[1]]))
  mat <- do.call(rbind, lapply(x, function(y) do.call(rbind, unclass(y))))
  colnames(mat) <- colnames0
  sm <- split_mat(mat)
  tibble::new_tibble(sm, nrow = length(sm[[1L]]))
}
#' @export
sc_coord.sfc_LINESTRING <- sc_coord.sfc_MULTIPOINT <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x[[1]]))
  mat <- do.call(rbind, unclass(x))
  colnames(mat) <- colnames0
  tibble::as_tibble(mat)
}
#' @export
sc_coord.sfc_POINT <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x[[1]]))
  mat <- do.call(rbind, unclass(x))
  colnames(mat) <- colnames0
  tibble::as_tibble(mat)
}

#' @name sc_coord
#' @export
#' @importFrom  stats setNames
sc_coord.MULTIPOLYGON <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x))
  tibble::as_tibble(setNames(dplyr::bind_rows(lapply(x, function(y) dplyr::bind_rows(lapply(y, sfcoords)))), colnames0))
  #setNames(purrr::map_df(x, function(y) purrr::map_df(y, sfcoords)), colnames)
}
#' @name sc_coord
#' @export
sc_coord.POLYGON <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x))
  setNames(dplyr::bind_rows(lapply(x, sfcoords)), colnames0)
  # setNames(purrr:map_df(x, sfcoords), colnames)
}
#' @name sc_coord
#' @export
sc_coord.MULTILINESTRING <- sc_coord.POLYGON
#' @name sc_coord
#' @export
sc_coord.LINESTRING <- function(x, ...) {
  colnames0 <- sc_geom_names(sf_geom_names(x))
  tibble::as_tibble(setNames(sfcoords(x), colnames0))
}
#' @name sc_coord
#' @export
sc_coord.MULTIPOINT <- sc_coord.LINESTRING
#' @name sc_coord
#' @export
sc_coord.POINT <- sc_coord.LINESTRING

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
#' @name sc_coord
#' @importFrom stats setNames
sc_coord.Spatial <- function(x, ...) {
  mat <- do.call(rbind, lapply(.sp_get_geometry(x), sc_coord))
  colnames(mat) <- c("x_", "y_")
  tibble::as_tibble(mat)
}
#' @name sc_coord
#' @export
sc_coord.Polygons <- function(x, ...){
  do.call(rbind, lapply(x@Polygons, function(xa) xa@coords))
}
#' @name sc_coord
#' @export
sc_coord.Lines<- function(x, ...){
  do.call(rbind, lapply(x@Lines, function(xa) xa@coords))
}


## --------------------------------------------------------
## trip
## --------------------------------------------------------

#' @export
sc_coord.trip <- function(x, ...) {
  tor <- slot(x, "TOR.columns")
  dt <- x[[tor[1L]]]
  data <- slot(x, "data")
  data <- data[setdiff(names(data), tor)]
  x <- tibble::as_tibble(slot(x, "coords"))
  x <- stats::setNames(x, c("x_", "y_"))
  x[["t_"]] <- dt
  dplyr::bind_cols(x, data)

}

## --------------------------------------------------------
## ade
## --------------------------------------------------------

#' @export
sc_coord.ltraj <- function(x, ...) {
  tibble::as_tibble(cbind(do.call("rbind", lapply(x, function(a) a[c("x", "y", "date")])),
                          do.call("rbind", lapply(x, function(a) attr(a, "infolocs")))))
}

