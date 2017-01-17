#' @importFrom utils head
path_to_segment <- function(x, id = NULL) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  x <- stats::setNames(tibble::as_tibble(utils::head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)), 
           c(".vertex0", ".vertex1"))
  if (!is.null(id)) x[["branch_"]] <- id
  x
}

#' Given a `BRANCH`` model decompose to 1-dimensional primitives. 
#' 
#' @param x input object
#' @param ... arguments passed to methods
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
  segment_longform <- dplyr::bind_rows(lapply(split(all_coordinates, all_coordinates$branch_),
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
#' @importFrom dplyr select
#' @examples
#' 
#' sf_obj <- inlandwaters[5, ]
#' sf_pslg <- PRIMITIVE(sf_obj)
#' plot(sf_pslg$vertex[, c("x_", "y_")], type = "n")
#' library(dplyr)
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
  x <- BRANCH(x) %>% PRIMITIVE()
  class(x) <-  c("PRIMITIVE", class(x))
  x
}

#' Recompose `sf` simple features from `PRIMITIVE`` models. 
#'
#' @param x input object
#' @param ... arguments passed to methods
#'
#' @return `PRIMITIVE`
#' @export
#'
#' @examples
#' prim <- PRIMITIVE(inlandwaters)
#' library(sf)
#' plot(st_as_sf(prim))
#' @importFrom sf st_as_sf st_multipolygon st_sfc 
#' @importFrom dplyr select inner_join
st_as_sf.PRIMITIVE <- function(x, ...) {
  ol <- vector("list", nrow(x$object))
  for (i_obj in seq(nrow(x$object))) {
   branch <- x$object[i_obj, ] %>% dplyr::select(object_) %>% 
     inner_join(x$branch) 
   brl <- vector("list", nrow(branch))
    for (i_br in seq(nrow(branch))) {
      br_0 <- branch[i_br, ] %>% 
     inner_join(x$branch_link_vertex) %>% 
     inner_join(x$vertex) %>% 
     split(.$island_)
      ## not getting holes properly
     brl[[i_br]] <- lapply(br_0, function(aa) as.matrix(aa[c(seq_len(nrow(aa)), 1L), c("x_", "y_")]))
    }
   ## slow, need to class all the structure without going through sf checks
   ol[[i_obj]] <- sf::st_multipolygon(brl)
  }
  ## TODO: need round-trip crs
  sfd <- as.data.frame(x$object)
  sfd[["geometry"]] <- sf::st_sfc(ol)
  sf::st_as_sf(sfd)
}