prim1d <- function(x, ...) {
  UseMethod("prim1d")
}

## I thought this was the analog to sc_path
## but has no purpose until the vertices are dense
## because the structural vertex index has no identity
sc_segment0 <- function(x, ...) {
  gm <- gibble::gibble(x)
  ## explode the map of segment to sc_coord
  dplyr::bind_rows(lapply(split(seq_len(sum(gm$nrow)), rep(seq_len(nrow(gm)), gm[["nrow"]])), 
         function(x) path_to_segment(x)), .id = "path")
  
}

## this is the literal segments by path
sc_segment <- function(x, ...) {
  #path <- PATH(x)[["path_link_vertex"]]
  path <- x[["path_link_vertex"]]
  dplyr::bind_rows(lapply(split(path[["vertex_"]], path[["path"]])[unique(path[["path"]])], 
                          function(x) path_to_segment(x)), .id = "path")
}

EDGE <- function(x, ...) {
  out <- PATH(x)
  seg <- sc_segment(out)
 uu <- paste(pmin(seg[[".vertex0"]], seg[[".vertex1"]]), 
                       pmax(seg[[".vertex0"]], seg[[".vertex1"]]), sep = "_")
  seg[["edge"]] <- sc_uid(length(unique(uu)))[factor(uu)]
  out[["object_link_edge"]] <-dplyr::transmute(seg, path, edge = seg[["edge"]]) %>% 
    inner_join(out$path) %>% select(object, edge)
  out[["edge_link_vertex"]] <- seg %>% 
    select(-path) %>%  
    distinct(edge, .keep_all = TRUE) #%>% 
 segment <- tibble::tibble(segment = match(seg$edge, out[["edge_link_vertex"]][["edge"]]))
 out[["path_link_vertex"]] <- NULL
 out[["path"]] <- NULL
  out
}
prim1d.PATH <- function(x, ...) {
  x$segment <- sc_primitive(x)
  x$path_link_vertex <- NULL
  x
}


sc_edge.PRIMITIVE <- function(x, ...) {
  u1 <- purrr::map(split(x$path_link_vertex, x$path_link_vertex$path), p2seg)
  u2 <- dplyr::mutate(dplyr::bind_rows(u1, .id = "path"), edge_ = row_number())
  u2[["uu"]] <- paste(pmin(u2[[".vertex0"]], u2[[".vertex1"]]), pmax(u2[[".vertex0"]], u2[[".vertex1"]]), sep = "_")
  dplyr::distinct(u2, .keep_all = TRUE)
}

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


get_shared_edge <- function(x, ...) UseMethod("get_shared_edge")
get_shared_edge.sf <- function(x, ...) {
  get_shared_edge(silicate::PRIMITIVE(x))
  
}
get_shared_edge.PRIMITIVE <- function(x, ...) {
  shared <- x$segment %>% dplyr::select(.vertex0, .vertex1) %>% 
    dplyr::mutate_all(dplyr::funs("I" = as.integer(factor(.))))  %>% 
    mutate(edge_id = paste(pmin(.vertex0_I, .vertex1_I), pmax(.vertex0_I, .vertex1_I), sep = "_")) %>% 
    group_by(edge_id) %>% filter(n() > 1) %>% ungroup() %>% select(.vertex0, .vertex1, edge_id)
  
  ## use this information to generate sf output
  #g0 <- shared %>% inner_join(x$vertex, c(".vertex0" = "vertex_")) %>% split(.$edge_id) %>% 
  #  purrr::map(function(x) st_linestring(as.matrix(x %>% dplyr::select(x_, y_)))) %>% st_sfc()
  #st_sf(a = seq_along(g0), geometry = g0)
  shared
}


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

find_arc0 <- function(path, candidates) {
  candidates <- candidates[candidates %in% path]
  if (length(candidates) == 0) return(NULL)
  index <- sort(match(candidates, path))
  mask <- logical(length(path))
  mask[index] <- TRUE
  arc_runs <- cumsum(c(0, abs(diff(mask))))
  out_index <- seq_along(arc_runs)
  ## wrap around the loop
  if (min(index) > 1) {
   arc_runs[ arc_runs ==arc_runs[1]] <- arc_runs[length(arc_runs)]
   out_index <- c(min(index):length(arc_runs), 1:(min(index)-1))
   
  } 

  out <- tibble::tibble(arc = arc_runs, vertex_ = path)
  f <- factor(out[["arc"]])
  out[["arc"]] <- sc_uid(nlevels(f))[f]
  out <- dplyr::slice(out, out_index) %>% dplyr::distinct(arc, vertex_)
  if (nlevels(f) ==1L) return(out)
  list_split_arcs <- split(out, out[["arc"]])
  listn <- length(list_split_arcs)
  for (i in seq(1, listn)) {
    list_split_arcs[[i]] <- dplyr::bind_rows(
      tibble::tibble(arc = list_split_arcs[[i]][["arc"]][1], vertex_ = tail(list_split_arcs[[i - 1 %% listn + 1]][["vertex_"]], 1)),
      list_split_arcs[[i]], 
      tibble::tibble(arc = list_split_arcs[[i]][["arc"]][1], vertex_ = head(list_split_arcs[[i %% listn + 1]][["vertex_"]], 1)))
  }
  dplyr::bind_rows(list_split_arcs) %>% dplyr::distinct()
}

find_arc <- function(path, candidates) {
  candidates <- candidates[candidates %in% path]
  if (length(candidates) == 0) return(NULL)
  index <- sort(match(candidates, path))
  ## wrap the path to start on a node
  if (min(index) > 1L) {
    path <- c(path[index[1L]:length(path)], path[1:(index[1L]-1)])    
    index <- sort(match(candidates, path))
  }
  mask <- logical(length(path))
  mask[index] <- TRUE
  arc_runs <- cumsum(c(0, abs(diff(mask))))
  out_index <- seq_along(arc_runs)

  out <- tibble::tibble(arc = arc_runs, vertex_ = path)
  f <- factor(out[["arc"]])
  out[["arc"]] <- sc_uid(nlevels(f))[f]
  if (nlevels(f) ==1L) return(out)
  list_arcs <- split(out_index, arc_runs)
  listn <- length(list_arcs)
  for (i in 2:listn) {
    list_arcs[[i]] <- c(tail(list_arcs[[i-1]], 1), 
                        list_arcs[[i]], 
                        head(list_arcs[[i %% listn + 1]], 1))
    
  }
  list_arc_id <- rep(levels(f), lengths(list_arcs))
  
  out <- tibble::tibble(arc = list_arc_id, vertex_ = path[unlist(list_arcs)])
  f <- factor(out[["arc"]])
  out[["arc"]] <- sc_uid(nlevels(f))[f]
  
  #out <- dplyr::slice(out, out_index) %>% dplyr::distinct(arc, vertex_)
  out
}

sc_arc_base <- function(path_link_vertex, node) {
  noded_path <- inner_join(node, path_link_vertex, "vertex_") %>% 
    dplyr::distinct(path)
  ## any paths node linked to nodes are just arcs
  arcs0 <- path_link_vertex %>% 
    anti_join(noded_path, "path")
  f <- factor(arcs0[["path"]])
  arcs0[["path"]] <- NULL
  arcs0[["arc"]] <- sc_uid(nlevels(f))[f]
  arcs0 <- dplyr::select(arcs0, arc, vertex_)
  path_link_vertex <- path_link_vertex %>% inner_join(noded_path)
  paths <- split(path_link_vertex, path_link_vertex[["path"]])
  bind_rows(arcs0, lapply(paths, function(x) find_arc(x[["vertex_"]], node[["vertex_"]])))
}
