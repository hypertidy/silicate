devtools::load_all()
obj <- sf::read_sf(system.file("etc/shapes/columbus.shp",
                               package="spdep"))
#obj <- polymesh
x <- PATH(obj)
library(sf)
plot(obj)

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
path_link_vertex %>% 
  inner_join(vertex) %>%
  ggplot() + 
  geom_path(aes(x_, y_, group = path, colour = path)) + guides(colour = FALSE)


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
arcs$uarc <- rep(dige, rle(arcs$arc)$lengths)
arcs_one <- arcs %>% distinct(uarc, .keep_all = TRUE) %>% dplyr::select(arc)


arcs %>% 
  inner_join(arcs_one, "arc") %>% 
  inner_join(vertex) %>% 
  ggplot() + geom_path(aes(x_, y_, group = arc, colour = arc)) + guides(colour = FALSE)

## convert the arcs to sf lines
gm <- arcs %>% inner_join(arcs_one, "arc") %>% 
  group_by(arc) %>% tally() %>% 
  rename(path= arc, nrow = n) %>% 
  mutate(type = "LINESTRING", ncol = 2, object = row_number())

## see how arc needs to be arranged, because of the group/tally for the gibble geom map
arc_sf <- build_sf(gm, coords_in = arcs %>%   
                     inner_join(arcs_one, "arc") %>% 
                     inner_join(vertex) %>% 
                     arrange(arc) %>% dplyr::select(x_, y_))
library(sf)

## arcs are now normalized
plot(st_sf(geom = arc_sf, 
           a = seq_along(arc_sf)), col = sample(rainbow(length(arc_sf))), 
     lwd = sample(1:7, length(arc_sf), replace = TRUE))

#############################################################
## TODO convert to simplicial complex form
## Segment/vertex form for Triangle
## unique edge form for CGAL
## OSM forms that are delivered this way
## maps package
## Atlantis BGM







