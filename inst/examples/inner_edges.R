## topology for simple features
## - must be added in
## - is ever needed

## rmapshaper has a very specific function to get the "inner lines" of a
## polygon mesh, but it's just the geometry no identity of which object
## or part its from
library(sf)
xsf <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  ib <- rmapshaper::ms_innerlines(xsf)

plot(ib, col = sample(viridis::viridis(length(ib))))

## here we use the silicate PATH model and the new functions sc_segment
## with its edge classification to reconstruct an sf object where all
## individual line segments know their parent/s from the PATH model
system.time({
  
library(tidyr)
library(silicate)
path <- PATH(xsf)
library(dplyr)
## arcs are all rings, not just shared
#arc <- sc_arc(path)
## segments are all instances, classified by edge groups
## and these are what we want
segs <- sc_segment(path) %>% 
        group_by(edge_) %>% mutate(n_edge = n()) %>% 
  dplyr::filter(n_edge > 1) %>% ungroup()
segs
## we only want to draw them once, but knowing which of two paths we belong to
## is key
shared <- segs %>% select(path_, edge_) %>% 
  group_by(edge_) %>% 
  mutate(i = sprintf("path%i", row_number())) %>% spread(i, path_)

build_sf_from_path <- function(x) {
  g <- tibble::tibble(vertex_ = unlist(x[c(".vertex0", ".vertex1")])) %>% 
    inner_join(path$vertex, "vertex_") %>% dplyr::select(x_, y_) %>% as.matrix() %>% sf::st_linestring()
  sf::st_sf(edge = x$edge_, path1 = x$path1, path2 = x$path2, geometry = st_sfc(g))
}
})

## reconstruct the inner edges
d <- do.call(rbind, shared %>% inner_join(segs, "edge_") %>% 
               purrr::transpose() %>% purrr::map(build_sf_from_path))

## consider now that we know path1 and path2 for every edge, but they don't 
## necessarily identify closed rings (since there are two choices for that)
library(ggplot2)
st_crs(d) <- st_crs(xsf)
ggplot(d) + geom_sf(aes(colour = path1)) + coord_sf(datum = NA) + guides(colour = FALSE)
ggplot(d) + geom_sf(aes(colour = path2)) + coord_sf(datum = NA) + guides(colour = FALSE)

## a second plot with the original boundaries first, because it looks neat
ggplot(xsf) + geom_sf() +  geom_sf(data = d, aes(colour = path1)) + coord_sf(datum = NA) + guides(colour = FALSE)


