# build segment table segtab (sc_segment on a path)
# classify as edges with unique-id, orientation edgtab (sc_edge(segtab))
# identify nodes within the vertex pool nodtab (sc_node(edgtab))
# identify arcs, paths of edges that end at nodes arctab (sc_arc(edgtab, nodtab))
# and identify all other paths 
# NARC is the graph of arc-node
# PRIM is the graph of 1D edges



# Arc node model, arc-node topology. 
# 
#
# @param x input model
# @param ... arguments to methods
#
# @return `tbl_df` of the node coordinates
# @export 
# @importFrom dplyr bind_rows mutate row_number distinct select inner_join group_by filter ungroup 


#' Unique edges for arc-node topology. 
#' 
#' So-called "arcs" are unclosed paths that end in nodes, vertices shared by other arcs. 
#' @param x input object
#' @param ... arguments for methods
#' @name sc_edge
#' @export
sc_edge <- function(x, ...) {
  UseMethod("sc_edge")
}
#' @name sc_edge
#' @export
sc_edge.default <- function(x, ...) {
  x <- PATH(x, ...)
  sc_edge(x)
}
#' @name sc_edge
#' @export
sc_edge.PATH <- function(x, ...) {
  sc_segment(x, ...) %>% dplyr::distinct(.data$edge_, .keep_all = TRUE) %>% 
    dplyr::select(.data$.vertex0, .data$.vertex1, .data$edge_)
}


#' Given a `PATH`` model decompose to 1-dimensional primitives (or 0-dimensional). 
#' 
#' @param x input object
#' @param ... arguments passed to methods
#' @importFrom dplyr %>% inner_join mutate 
#' @name sc_segment
#' @export
sc_segment <- function(x, ...) UseMethod("sc_segment")
#' @name sc_segment
#' @export
sc_segment.default <- function(x, ...) {
  x <- PATH(x)
  sc_segment(x, ...)
}
#' @name sc_segment
#' @export
sc_segment.PATH <- function(x, ...) {
 sc_segment_base(x[["path_link_vertex"]]) 
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
#' @name sc_node
#' @export
sc_node.default <- function(x, ...) {
  x <- PATH(x)
  sc_node(x)
}
#' @name sc_node
#' @export
sc_node.PATH <- function(x, ...) {
  sc_node_base(sc_edge(x), x[["vertex"]])
}
 

#' Arcs for arc-node topology. 
#' 
#' Arcs are unbranched paths within the line segment graph. Nodes are the vertices where three or more arcs meet. 
#' @param x input object
#' @param ... arguments for methods
#' @export
sc_arc <- function(x, ...) {
  UseMethod("sc_arc")
}
#' @name sc_arc
#' @export
sc_arc.default <- function(x, ...) {
  x <- PATH(x)
  sc_arc(x, ...)
}
#' @name sc_arc
#' @export
sc_arc.PATH <- function(x, ...) {
  sc_arc_base(x[["path_link_vertex"]], sc_node(x))
}
path_to_segment0 <- function(x) faster_as_tibble(list(.vertex0 = utils::head(x, -1L), 
                                                      .vertex1 = utils::tail(x, -1)))

sc_segment_base <- function(path_link_vertex) {
  ## how many vertices on each path?
  frle <- rle(path_link_vertex[["path_"]])
  ## push into segments 
  segtab <- path_to_segment0(path_link_vertex[["vertex_"]])
  segtab <- segtab[-head(cumsum(frle$lengths), -1L), ]
  segtab[["path_"]] <- rep(frle$values, frle$lengths - 1)
  segtab[["segment_"]] <- sc_uid(nrow(segtab))
  edge <- paste(pmin(segtab[[".vertex0"]], segtab[[".vertex1"]]), pmax(segtab[[".vertex0"]], segtab[[".vertex1"]]), sep = "_")
  f <- factor(edge)
  segtab[["edge_"]] <- sc_uid(nlevels(f))[f]
  segtab
}
#' @importFrom rlang .data
sc_node_base <- function(unique_edges, vertex, ...) {
  nodes <- dplyr::bind_rows(vertex %>% dplyr::select(.data$vertex_) %>% 
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     vertex %>% dplyr::select(.data$vertex_) %>% 
                       dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(.data$edge_, .data$vertex_) %>% 
    dplyr::group_by(.data$vertex_) %>% dplyr::tally() %>% dplyr::ungroup() %>% 
    dplyr::filter(.data$n > 2) %>% dplyr::distinct(.data$vertex_) 
  nodes
}

find_arc <- function(path, nodes) {
  path$candidate <- path$vertex_ %in% nodes
  
  ## rewind to put a node at the top
  idx <- which(path$candidate)
  if (idx[1L] > 1L) {
    path <- bind_rows(path[idx[1L]:nrow(path), ], 
                   path[1L:(idx[1L] - 1L), ]) %>% dplyr::distinct()
    path <- bind_rows(path, path %>% dplyr::slice(1L))
    idx <- which(path$candidate)
  }
  
  arcs <- vector("list", length(idx) - 1)
  for (i in seq_along(arcs)) {
    arcs[[i]] <- seq(idx[i], idx[i+1])
  }
  
  tibble::tibble(arc_ = sc_uid(length(arcs))[rep(seq_along(arcs), lengths(arcs))], 
                 vertex_ = path[["vertex_"]][unlist(arcs)])
}
sc_arc_base <- function(path_link_vertex, node) {
  noded_path <- dplyr::inner_join(node, path_link_vertex, "vertex_") %>% 
    dplyr::distinct(.data$path_)
  ## any paths not linked to nodes are just arcs
  ## (there may be none)
  arcs0 <- path_link_vertex %>% 
    dplyr::anti_join(noded_path, "path_")
  f <- factor(arcs0[["path_"]])
  arcs0[["path_"]] <- NULL
  arcs0[["arc_"]] <- sc_uid(nlevels(f))[f]
  arcs0 <- dplyr::select(arcs0, .data$arc_, .data$vertex_)
  path_link_vertex <- path_link_vertex %>% inner_join(noded_path, "path_")
  paths <- split(path_link_vertex, path_link_vertex[["path_"]])
  bind_rows(arcs0, lapply(paths, function(x) find_arc(x, node[["vertex_"]])))
}
