
## https://CRAN.R-project.org/package=spdep/vignettes/nb_igraph.html


## this function uniquifies the segments, very much WIP
u_edges <- function(x, ...) UseMethod("u_edges")
u_edges.PRIMITIVE <- function(x, ...) {
  u_edges(x[["segment"]])
}
u_edges.data.frame <- function(x, ...) {
  u2 <- x %>%         mutate(uu = paste(pmin(.vertex0, .vertex1), pmax(.vertex0, .vertex1), sep = "_"))
  #dplyr::distinct(select(u2, uu, segment_), uu,  .keep_all = TRUE)
  select(u2, uu, segment_)
}

#' find neighbours
#' 
#' Provides a nested tibble of row-indexes for both vertices and edges. 
#' No topological fixing or fudging is done here. 
find_nb <- function(x) {
  etab <- vtab <- vector("list", nrow(x$object))
  for (ith in seq_along(etab)) {
  xith <- x$object[ith, "object_"] %>% inner_join(x$path, "object_") %>% 
      inner_join(x$path_link_vertex, "path_") %>% 
      inner_join(x$vertex, "vertex_")
  
  ## join by vertex 
  idx_vertex <- xith %>% dplyr::select(vertex_) %>% 
    inner_join(x$path_link_vertex, "vertex_") %>% 
    inner_join(x$path, "path_") %>% 
    distinct(object_) ##%>% inner_join(nc %>% mutate(object_ = x$object_))
  v_idx <- match(idx_vertex$object_, x$object$object_)
  
  
  ## join the subset data to the main on unique segment 
  ## (unique as in order of vertices is irrelevant)
  idx_edge <- u_edges.data.frame(x$object[ith, "object_"] %>% 
                                   inner_join(x$path, "object_") %>% 
                                   inner_join(x$segment, "path_")) %>%  
    select(-segment_) %>% inner_join(u_edges.PRIMITIVE(x), "uu") %>% 
    inner_join(x$segment, "segment_") %>% inner_join(x$path, "path_") %>% 
    distinct(object_)
  e_idx <- match(idx_edge$object_, x$object$object_)
  
  ## remove the self link
  v_idx <- setdiff( v_idx, ith)
  e_idx <- setdiff(e_idx, ith)
  vtab[[ith]] <- tibble(neighbour = v_idx, object_ = ith)
  etab[[ith]] <- tibble(neighbour = e_idx, object_ = ith)
  }
  tibble(vnb = vtab, enb = etab)
}
 
#' build edge-based igraph from nested tibbles  (see find_nb)
g_from_nb <- function(x, layout = NULL) {
  g <- as_tibble(x) %>% dplyr::select(enb) %>% unnest() %>% select(object_, neighbour) %>% graph_from_data_frame()
  if (!is.null(layout)) {
    igraph::V(g)$x <- layout[[1]]
    igraph::V(g)$y <- layout[[2]]
  }
  g
}


library(dplyr)
library(tibble)
library(igraph)
library(scsf)
library(tidyr)
pp <- PRIMITIVE(minimal_mesh)


library(sf)
columbus <- sf::read_sf(system.file("etc/shapes/columbus.shp", package="spdep"))
nb_q <- spdep::poly2nb(as(columbus, "Spatial"))
str(nb_q)
prim <- PRIMITIVE(columbus)
d <- bind_cols(columbus, find_nb(prim) )

## the tidy print method
as_tibble(d) %>% select(geometry, vnb, enb)

## the coordinates of a centroid of each polygon
lyout <- as.data.frame(do.call(rbind, lapply(st_geometry(st_centroid(columbus)), function(x) unclass(x))))

as_tibble(d) %>% g_from_nb(layout = lyout) %>% plot(vertex.size = 0, edge.arrow.size = 0, asp = 0)
plot(lyout, type = "n", axes = FALSE, xlab = "", ylab = "", asp = "")
plot(nb_q, lyout, col="grey", add = TRUE)

