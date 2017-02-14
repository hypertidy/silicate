
#' NARC model, arc-node topology. 
#' 
#' Arcs are unbranched paths within the line segment graph. Nodes are the vertices where three or more arcs meet. 
#'
#' @param x input model
#' @param ... arguments to methods
#'
#' @return `tbl_df` of the node coordinates
#' @export 
#'
NARC <- function(x, ...) {
  UseMethod("NARC")
}

#' @name NARC
#' @export
#' @importFrom dplyr select_
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate distinct select inner_join group_by filter ungroup 
NARC.PRIMITIVE <- function(x, ...) {

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

#' Arcs for arc-node topology. 
#' 
#' So-called "arcs" are unclosed paths that end in nodes, vertices shared by other arcs. 
#' @param x input object
#' @param ... arguments for methods
#' @export
sc_arc <- function(x, ...) {
  UseMethod("sc_arc")
}
#' Nodes for arc-node topology. 
#' 
#' Nodes are the vertices in the graph that are shared by "arcs". 
#' 
#' @seealso sc_arc NARC
#' @param x input object
#' @param ... arguments for methods
#' @export
sc_node <- function(x, ...) {
  UseMethod("sc_node")
}
#' @name sc_arc
#' @export
sc_arc.default <- function(x, ...) {
  x <- PRIMITIVE(x, ...)
  sc_arc(x)
}
#' @name sc_arc
#' @export
sc_arc.PRIMITIVE <- function(x, ...) {
    unique_edges <- x$path_link_vertex %>% split(.$path_) %>% 
    purrr::map(p2seg) %>% 
    dplyr::bind_rows(.id = "path_")%>% dplyr::mutate(edge_ = row_number()) %>% 
    dplyr::mutate(uu = paste(pmin(.vertex0, .vertex1), pmax(.vertex0, .vertex1), sep = "_")) %>% 
    dplyr::distinct(uu, .keep_all = TRUE)
  unique_edges
}

#' @name sc_node
#' @export
sc_node.default <- function(x, ...) {
  x <- PRIMITIVE(x, ...)
  sc_node(x)
}
#' @name sc_node
#' @export
sc_node.PRIMITIVE <- function(x, ...) {
  unique_edges <- sc_arc(x, ...)
  nodes <- bind_rows(x$vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     x$vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(edge_, vertex_) %>% 
    dplyr::group_by(vertex_) %>% dplyr::mutate(nb = n()) %>% dplyr::ungroup() %>% 
    dplyr::filter(nb > 2) %>% distinct(vertex_) %>% dplyr::inner_join(x$vertex)
  nodes
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
#' A PRIMITIVES model is a decomposition of spatial forms to primitives. 
#' @param x input object
#' @param ... arguments passed to methods
#' @name PRIMITIVE
#' @export
PRIMITIVE <- function(x, ...) UseMethod("PRIMITIVE")
#' @name PRIMITIVE
#' @export
PRIMITIVE.PATH <- function(x, ...) {
  x$segment <- sc_primitive(x, ...)
  x
}
#' @name PRIMITIVE
#' @export
PRIMITIVE.default <- function(x, ...) {
  x <- PATH(x) %>% PRIMITIVE(...)
  class(x) <-  c("PRIMITIVE", class(x))
  x
}

