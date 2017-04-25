library(sf)
library(scsf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
prim <- PRIMITIVE(minimal_mesh)#[c(9, 15, 24,  5, 16, 31), ])
library(scgraph)
library(igraph)
library(ggraph)
# ig <- scgraph::sc_as_igraph(prim) 
# ## this is unlikely to be the right order
# E(ig)$segment <- prim$segment$segment_
# 
# ggraph(ig) + ggraph::geom_edge_link(aes(label = segment_))

## find vertices that are nodes
nodes <- sc_node(prim)

split_at <- function(x, vertex) {
  x$order <- seq(nrow(x))
  test <- x$vertex_ %in% vertex
  if (!any(test)) return(list(x))
  segs <- c(1, cumsum(abs(diff(test))))
  split(x, segs)
}
library(tidyverse)
arcs <- bind_rows(unlist(lapply(prim$path_link_vertex %>% split(.$path_), function(x) split_at(x, nodes$vertex_)), recursive = FALSE), .id = "arc")
#arcs <- arcs %>% distinct(path_, vertex_, .keep_all = TRUE)
ggplot(arcs %>%  left_join(prim$vertex) %>% arrange(arc, order), 
       aes(x_, y_, group = arc, colour = arc)) + geom_path(lwd = 3) + guides(colour = FALSE)  

plot(igraph::induced_subgraph(ig, nodes$vertex_) )
# ## process all paths into arcs
# 
# all_paths <- sc_path(prim) %>% select(path_, object_) #distinct(path_)
# for (ipath in seq_len(nrow(all_paths))) {
#   all_paths %>% slice(ipath) %>% left_join(prim$path_link_vertex) %>% inner_join(nodes)
# }