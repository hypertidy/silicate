

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
