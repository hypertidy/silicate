
#' Arc-node topology. 
#' 
#' Arcs are unbranched paths within the line segment graph. Nodes are the vertices where three or more arcs meet. 
#'
#' @param x input model
#' @param ... arguments to methods
#'
#' @return `tbl_df` of the node coordinates
#' @export 
#'
#' @examples
#' x <- sf::st_read(system.file("extdata/file.geojson", package= "sc"))
#' arc_node(x)  ## get the nodes
#' ## now get the arcs (should the functions be called arc() and node()?)
arc_node <- function(x, ...) {
  UseMethod("arc_node")
}
#' @name arc_node
#' @export
arc_node.sf <- function(x, ...) {
  arc_node(PRIMITIVE(x))
}
#' @name arc_node
#' @export
#' @importFrom dplyr select_
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate distinct select inner_join group_by filter ungroup 
arc_node.PRIMITIVE <- function(x, ...) {
  p2seg <- function(x) tibble::as_tibble(path_to_segment(x$vertex_))
  
  unique_edges <- x$path_link_vertex %>% split(.$path_) %>% 
    purrr::map(p2seg) %>% 
    dplyr::bind_rows(.id = "path_")%>% dplyr::mutate(edge_ = row_number()) %>% 
    dplyr::mutate(uu = paste(pmin(.vertex0, .vertex1), pmax(.vertex0, .vertex1), sep = "_")) %>% 
    dplyr::distinct(uu, .keep_all = TRUE)
  
  nodes <- bind_rows(x$vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     x$vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(edge_, vertex_) %>% 
    dplyr::group_by(vertex_) %>% dplyr::mutate(nb = n()) %>% dplyr::ungroup() %>% 
    dplyr::filter(nb > 2) %>% distinct(vertex_) %>% dplyr::inner_join(x$vertex)
  
  #plot(st_geometry(nc))
  #points(nodes$x_, nodes$y_)
  
 dplyr::select_(nodes, "x_", "y_", "vertex_")
 # nodes
}
#' @importFrom utils head
path_to_segment <- function(x, id = NULL) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  x <- stats::setNames(tibble::as_tibble(utils::head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)), 
           c(".vertex0", ".vertex1"))
  if (!is.null(id)) x[["path_"]] <- id
  x
}

#' Given a `PATH`` model decompose to 1-dimensional primitives. 
#' 
#' @param x input object
#' @param ... arguments passed to methods
#' sf_obj <- inlandwaters[5, ]
#' obj <- PATH(sf_obj)
#' prim <- sc_primitive(obj)
#' sf_pslg <- PRIMITIVE(sf_obj)
#' @importFrom dplyr %>% inner_join mutate 
sc_primitive <- function(x, ...) UseMethod("sc_primitive")
sc_primitive.PATH <- function(x, ...) {
  v <- x$vertex 
  bXv <- x$path_link_vertex
  all_coordinates <- dplyr::inner_join(bXv, v, "vertex_")
  
  ## this is a subset of RTriangle::pslg (because the original target was RTriangle::triangulate)
  #pstraight_lgraph <- list(P = as.matrix(v[, c("x_", "y_")]),
  ## only need S for 1D
  segment_longform <- dplyr::bind_rows(lapply(split(all_coordinates, all_coordinates$path_),
                                         function(x) path_to_segment(x$vertex_, x$path_[1L])))
  
  
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
#' ## find nodes?
#' nc_pslg$vertex 
#' @name PRIMITIVE
#' @export
PRIMITIVE <- function(x, ...) UseMethod("PRIMITIVE")
#' @name PRIMITIVE
#' @export
PRIMITIVE.PATH <- function(x, ...) {
  x$segment <- sc_primitive(x)
  x
}
#' @name PRIMITIVE
#' @export
PRIMITIVE.sf <- function(x, ...) {
  x <- PATH(x) %>% PRIMITIVE()
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
   path <- x$object[i_obj, ] %>% dplyr::select(object_) %>% 
     inner_join(x$path) 
   brl <- vector("list", nrow(path))
    for (i_br in seq(nrow(path))) {
      br_0 <- path[i_br, ] %>% 
     inner_join(x$path_link_vertex) %>% 
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