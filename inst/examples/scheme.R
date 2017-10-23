#devtools::load_all()
obj <- minimal_mesh
x <- PATH(obj)


## PATH is created by doing these steps

## non-normalized vertex pool
coord <- sc_coord(obj)
## map of instances of vertices in the original structure
gmap <- gibble::gibble(obj)

## normalize the vertices (a coordinate is an instance of a vertex)
coordspace <- c("x_", "y_")
## classify all coordinates by unique instances (not robust, assumes 100% accurate)
pastey <- function(...) paste(..., sep = "-")
coordclass <- factor(do.call(pastey, lapply(coord[coordspace], function(x) as.character(x))))
library(dplyr)
vertex <- coord[!duplicated(coordclass), ] %>% 
  dplyr::mutate(vertex_ = row_number())
path_link_vertex <- tibble::tibble(path = rep(sc_uid(nrow(gmap)), gmap[["nrow"]]), vertex_ = match(coordclass, coordclass[!duplicated(coordclass)]))


library(ggplot2)
library(ggpolypath)
path_link_vertex %>% 
  inner_join(vertex) %>%
  ggplot() + 
  geom_polypath(aes(x_, y_, group = path, col = path))


## what is PATH good for?

#############################################################
## convert to arc-node
library(dplyr)
edges <- sc_segment_base(path_link_vertex) %>% 
  distinct(edge, .keep_all = TRUE)
node <- sc_node_base(edges, vertex) %>% 
  inner_join(vertex, "vertex_") %>% 
  dplyr::select(x_, y_, vertex_)
arcs <- sc_arc_base(path_link_vertex, node = node) 

uarcs <- unique(arcs$arc)
dige <- character(length(uarcs))
for (i in seq_along(uarcs)) dige[i] <- digest::digest(sort(arcs$vertex_[arcs$arc == uarcs[i]]))
arcs$uarc <- dige
arcs <- arcs %>% distinct(uarc, .keep_all = TRUE)
## convert the arcs to sf lines
gm <- arcs %>% group_by(arc) %>% tally() %>% 
  rename(path= arc, nrow = n) %>% 
  mutate(type = "LINESTRING", ncol = 2, object = row_number())

## see how arc needs to be arranged, because of the group/tally for the gibble geom map
arc_sf <- build_sf(gm, coords_in = arcs %>% inner_join(x$vertex) %>% arrange(arc) %>% dplyr::select(x_, y_))
library(sf)

## arcs aren't normalized
plot(st_sf(geom = arc_sf, 
           a = seq_along(arc_sf)), col = rainbow(length(arc_sf)), 
     lwd = sample(length(arc_sf)))

#############################################################
## convert to simplicial complex form








