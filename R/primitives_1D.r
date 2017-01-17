path_to_segment <- function(x, id = NULL) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  x <- stats::setNames(tibble::as_tibble(head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)), 
           c(".vertex0", ".vertex1"))
  if (!is.null(id)) x[["branch_"]] <- id
  x
}

#' Given a `BRANCH`` model decompose to 1-dimensional primitives. 
#sc_object, sc_branch, sc_coord
#' @examples
#' 
#' sf_obj <- inlandwaters[5, ]
#' obj <- BRANCH(sf_obj)
#' prim <- sc_primitive(obj)
#' sf_pslg <- PRIMITIVE(sf_obj)
#' @importFrom dplyr %>% inner_join mutate 
sc_primitive <- function(x, ...) UseMethod("sc_primitive")
sc_primitive.BRANCH <- function(x, ...) {
  v <- x$vertex 
  bXv <- x$branch_link_vertex
  all_coordinates <- dplyr::inner_join(bXv, v, "vertex_")
  
  ## this is a subset of RTriangle::pslg (because the original target was RTriangle::triangulate)
  #pstraight_lgraph <- list(P = as.matrix(v[, c("x_", "y_")]),
  ## only need S for 1D
  segment_longform <- dplyr::bind_rows(lapply(all_coordinates %>% split(.$branch_),
                                         function(x) path_to_segment(x$vertex_, x$branch_[1L])))
  
  
  segment_longform[["segment_"]] <- sc_rand(n = nrow(segment_longform))     
  
  segment_longform
}

#' Generate a PRIMITIVES model. 
#' 
#' A PRIMITIVES model is a decomposition of spatial forms to primitives. In the case
#' of POLYGON and MULTIPOLYGON and LINESTRING and MULTILINESTRING types this is the 
#' set of two-coordinate line segments. 
#' 
#' @param x input object
#' @param ... arguments passed to methods
#' @examples
#' 
#' sf_obj <- inlandwaters[5, ]
#' sf_pslg <- PRIMITIVE(sf_obj)
#' plot(sf_pslg$vertex[, c("x_", "y_")], type = "n")
#' P1_segments <- function(x, ...) {
#'   s1 <- x$segment %>% 
#'       inner_join(x$vertex, c(".vertex0" = "vertex_")) %>% 
#'       select(x_, y_)
#'   s2 <- x$segment %>% 
#'       inner_join(x$vertex, c(".vertex1" = "vertex_")) %>% 
#'       select(x_, y_)
#'    graphics::segments(s1$x_, s1$y_, s2$x_, s2$y_, ...)
#' }
#' P1_segments(sf_pslg)
#' nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
#' plot(sf::st_geometry(nc), border = "white")
#' nc_pslg <- PRIMITIVE(nc)
#' P1_segments(nc_pslg)
#' 
#' @name PRIMITIVE
#' @export
PRIMITIVE <- function(x, ...) UseMethod("PRIMITIVE")
#' @name PRIMITIVE
#' @export
PRIMITIVE.BRANCH <- function(x, ...) {
  x$segment <- sc_primitive(x)
  x
}
#' @name PRIMITIVE
#' @export
PRIMITIVE.sf <- function(x, ...) {
  x <- BRANCH(x) %>% PRIMITIVE(br)
  class(x) <-  c("PRIMITIVE", class(x))
  x
}