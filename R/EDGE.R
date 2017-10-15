# build segment table segtab (sc_segment on a path)
# classify as edges with unique-id, orientation edgtab (sc_edge(segtab))
# identify nodes within the vertex pool nodtab (sc_node(edgtab))
# identify arcs, paths of edges that end at nodes arctab (sc_arc(edgtab, nodtab))
# and identify all other paths 


sc_segment_base <- function(path_link_vertex) {
  #   u1 <- purrr::map(split(x$path_link_vertex, x$path_link_vertex$path), p2seg)
  #u2 <- dplyr::mutate(dplyr::bind_rows(u1, .id = "path"), edge_ = row_number())
  
  segtab <- dplyr::bind_rows(lapply(split(path_link_vertex[["vertex_"]], path_link_vertex[["path"]])[unique(path_link_vertex[["path"]])], 
                          function(x) path_to_segment(x)), .id = "path")
  segtab[["segment"]] <- sc_uid(nrow(segtab))
  edge <- paste(pmin(segtab[[".vertex0"]], segtab[[".vertex1"]]), pmax(segtab[[".vertex0"]], segtab[[".vertex1"]]), sep = "_")
  f <- factor(edge)
  segtab[["edge"]] <- sc_uid(nlevels(f))[f]
  segtab
}

sc_node_base <- function(unique_edges, vertex, ...) {
  nodes <- dplyr::bind_rows(vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex0")), 
                     vertex %>% dplyr::select(vertex_) %>% dplyr::inner_join(unique_edges, c("vertex_" = ".vertex1"))) %>% 
    dplyr::distinct(edge, vertex_) %>% 
    dplyr::group_by(vertex_) %>% dplyr::tally() %>% dplyr::ungroup() %>% 
    dplyr::filter(n > 2) %>% dplyr::distinct(vertex_) 
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
  noded_path <- inner_join(node, path_link_vertex, "vertex_") %>% 
    dplyr::distinct(path)
  ## any paths not linked to nodes are just arcs
  ## (there may be none)
  arcs0 <- path_link_vertex %>% 
    anti_join(noded_path, "path")
  f <- factor(arcs0[["path"]])
  arcs0[["path"]] <- NULL
  arcs0[["arc"]] <- sc_uid(nlevels(f))[f]
  arcs0 <- dplyr::select(arcs0, arc, vertex_)
  path_link_vertex <- path_link_vertex %>% inner_join(noded_path)
  paths <- split(path_link_vertex, path_link_vertex[["path"]])
  bind_rows(arcs0, lapply(paths, function(x) find_arc(x, node[["vertex_"]])))
}
