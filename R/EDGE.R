# build segment table segtab (sc_segment on a path)
# classify as edges with unique-id, orientation edgtab (sc_edge(segtab))
# identify nodes within the vertex pool nodtab (sc_node(edgtab))
# identify arcs, paths of edges that end at nodes arctab (sc_arc(edgtab, nodtab))
# and identify all other paths 


path_to_segment0 <- function(x) faster_as_tibble(list(.vertex0 = utils::head(x, -1L), 
                                                      .vertex1 = utils::tail(x, -1)))

sc_segment_base <- function(path_link_vertex) {
  ## how many vertices on each path?
  frle <- rle(path_link_vertex[["path"]])
  ## push into segments 
  segtab <- path_to_segment0(path_link_vertex[["vertex_"]])
  segtab <- segtab[-head(cumsum(frle$lengths), -1L), ]
  segtab[["path"]] <- rep(frle$values, frle$lengths - 1)
  segtab[["segment"]] <- sc_uid(nrow(segtab))
  edge <- paste(pmin(segtab[[".vertex0"]], segtab[[".vertex1"]]), pmax(segtab[[".vertex0"]], segtab[[".vertex1"]]), sep = "_")
  f <- factor(edge)
  segtab[["edge"]] <- sc_uid(nlevels(f))[f]
  segtab
}
#' @importFrom rlang .data
sc_node_base <- function(unique_edges, vertex, ...) {
  nodes <- dplyr::bind_rows(vertex %>% dplyr::select(.data$vertex_) %>% 
                              dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     vertex %>% dplyr::select(.data$vertex_) %>% 
                       dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(.data$edge, .data$vertex_) %>% 
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
                   path[1L:(idx[1L] - 1L), ])
    idx <- which(path$candidate)
  }
  
  arcs <- vector("list", length(idx) - 1)
  for (i in seq_along(arcs)) {
    arcs[[i]] <- seq(idx[i], idx[i+1])
  }
  
  tibble::tibble(arc = sc_uid(length(arcs))[rep(seq_along(arcs), lengths(arcs))], 
                 vertex_ = path[["vertex_"]][unlist(arcs)])
}
sc_arc_base <- function(path_link_vertex, node) {
  noded_path <- dplyr::inner_join(node, path_link_vertex, "vertex_") %>% 
    dplyr::distinct(.data$path)
  ## any paths not linked to nodes are just arcs
  ## (there may be none)
  arcs0 <- path_link_vertex %>% 
    dplyr::anti_join(noded_path, "path")
  f <- factor(arcs0[["path"]])
  arcs0[["path"]] <- NULL
  arcs0[["arc"]] <- sc_uid(nlevels(f))[f]
  arcs0 <- dplyr::select(arcs0, .data$arc, .data$vertex_)
  path_link_vertex <- path_link_vertex %>% inner_join(noded_path, "path")
  paths <- split(path_link_vertex, path_link_vertex[["path"]])
  bind_rows(arcs0, lapply(paths, function(x) find_arc(x, node[["vertex_"]])))
}
