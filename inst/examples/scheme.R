devtools::load_all()
obj <- polymesh
x <- PATH(obj)
library(sf)
#plot(obj)

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
  geom_path(aes(x_, y_, group = path, colour = path, lwd = path)) + guides(colour = FALSE)


## what is PATH good for?

#############################################################
## convert to arc-node
library(dplyr)
arcs <- sc_arc(x) 

uarcs <- unique(arcs$arc_)
dige <- character(length(uarcs))
for (i in seq_along(uarcs)) dige[i] <- digest::digest(sort(arcs$vertex_[arcs$arc_ == uarcs[i]]))
arcs$uarc <- rep(dige, rle(arcs$arc_)$lengths)
arcs_one <- arcs %>% distinct(uarc, .keep_all = TRUE) %>% dplyr::select(arc_)


arcs %>% 
  #inner_join(arcs_one, "arc") %>% 
  inner_join(x$vertex) %>% 
  ggplot() + geom_path(aes(x_, y_, group = arc_, colour = arc_)) + guides(colour = FALSE)

## convert the arcs to sf lines
gm <- arcs %>% inner_join(arcs_one, "arc_") %>% 
  group_by(arc_) %>% tally() %>% 
  rename(path_= arc_, nrow = n) %>% 
  mutate(type = "LINESTRING", ncol = 2, object_ = row_number())

## see how arc needs to be arranged, because of the group/tally for the gibble geom map
arc_sf <- build_sf(gm, coords_in = arcs %>%   
                     inner_join(arcs_one, "arc_") %>% 
                     inner_join(x$vertex) %>% 
                     arrange(arc_) %>% dplyr::select(x_, y_))
library(sf)

## arcs are now normalized
plot(st_sf(geom = arc_sf, 
           a = seq_along(arc_sf)), col = sample(rainbow(length(arc_sf))), 
     lwd = sample(1:7, length(arc_sf), replace = TRUE))

#############################
## convert to segments only
objects <- sc_segment(x) %>% 
  inner_join(x$path) %>% 
  dplyr::select(.vertex0, .vertex1, path_, object_) 

tibble::tibble(vertex_ = purrr::transpose(objects %>% select(.vertex0, .vertex1)) %>% 
  unlist()) %>% inner_join(x$vertex)

 #############################################################
## TODO convert to simplicial complex form
## Segment/vertex form for Triangle
## unique edge form for CGAL
## OSM forms that are delivered this way
## maps package
## Atlantis BGM







