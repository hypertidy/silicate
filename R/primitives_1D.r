
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
#' @importFrom dplyr bind_rows mutate row_number distinct select inner_join group_by filter ungroup 
NARC.PRIMITIVE <- function(x, ...) {

  ## we only have sc_edge yet
  #x[["arcs"]] <- sc_arc(x)
  x[["nodes"]] <- sc_node(x)
 #dplyr::select_(nodes, "x_", "y_", "vertex_")
 x
}

#' Unique edges for arc-node topology. 
#' 
#' So-called "arcs" are unclosed paths that end in nodes, vertices shared by other arcs. 
#' @param x input object
#' @param ... arguments for methods
sc_edge <- function(x, ...) {
  UseMethod("sc_edge")
}
#' Nodes for arc-node topology. 
#' 
#' Nodes are the vertices in the graph that are shared by "arcs". 
#' 
#' @seealso NARC
#' @param x input object
#' @param ... arguments for methods
#' @export
sc_node <- function(x, ...) {
  UseMethod("sc_node")
}
#' @name sc_edge
sc_edge.default <- function(x, ...) {
  x <- PRIMITIVE(x, ...)
  sc_edge(x)
}
#' @name sc_edge
sc_edge.PRIMITIVE <- function(x, ...) {
    u1 <- purrr::map(split(x$path_link_vertex, x$path_link_vertex$path), p2seg)
    u2 <- dplyr::mutate(dplyr::bind_rows(u1, .id = "path"), edge_ = row_number())
    u2[["uu"]] <- paste(pmin(u2[[".vertex0"]], u2[[".vertex1"]]), pmax(u2[[".vertex0"]], u2[[".vertex1"]]), sep = "_")
    dplyr::distinct(u2, .keep_all = TRUE)
}

#' @name sc_node
#' @export
sc_node.default <- function(x, ...) {
  x <- PRIMITIVE(x, ...)
  sc_node(x)
}
#' @name sc_node
#' @importFrom dplyr distinct_ tally
#' @export
sc_node.PRIMITIVE <- function(x, ...) {
  unique_edges <- sc_edge(x, ...)
  nodes <- bind_rows(x$vertex %>% dplyr::select_("vertex_") %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     x$vertex %>% dplyr::select_("vertex_") %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct_("edge_", "vertex_") %>% 
    dplyr::group_by_("vertex_") %>% dplyr::tally() %>% dplyr::ungroup() %>% 
    dplyr::filter_(quote(n > 2)) %>% distinct_("vertex_") ##%>% dplyr::inner_join(x$vertex, "vertex_")
  nodes
}



#' Given a `PATH`` model decompose to 1-dimensional primitives (or 0-dimensional). 
#' 
#' @param x input object
#' @param ... arguments passed to methods
#' @importFrom dplyr %>% inner_join mutate 
sc_primitive <- function(x, ...) UseMethod("sc_primitive")
sc_primitive.PATH <- function(x, ...) {
  v <- x$vertex 
  bXv <- x$path_link_vertex
  all_coordinates <- dplyr::inner_join(bXv, v, "vertex_")
  
  ## what if we had points
  if (nrow(bXv) == nrow(distinct_(bXv, "path"))) {
    return(dplyr::transmute_(all_coordinates, .vertex0 = quote(vertex_), path = quote("path")))
  }
  ## this is a subset of RTriangle::pslg (because the original target was RTriangle::triangulate)
  #pstraight_lgraph <- list(P = as.matrix(v[, c("x_", "y_")]),
  ## only need S for 1D
  segment_longform <- dplyr::bind_rows(lapply(split(all_coordinates, all_coordinates$path),
                                         function(x) path_to_segment(x$vertex_, x$path[1L])))
  
  
  segment_longform[["segment_"]] <- sc_uid(n = nrow(segment_longform))     
  
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
  class(x) <- c("PRIMITIVE", class(x))
  x
}
#' @name PRIMITIVE
#' @export
PRIMITIVE.default <- function(x, ...) {
  x <- PATH(x) %>% PRIMITIVE(...)
  class(x) <-  c("PRIMITIVE", class(x))
  x
}

