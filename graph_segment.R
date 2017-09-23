columbus <- rgdal::readOGR(system.file("etc/shapes/columbus.shp",
                                       package="spdep"))
rook <- spdep::poly2nb(columbus, queen = FALSE)

prim <- silicate::PRIMITIVE(columbus)
library(tidyverse)
prim$segment %>% select(path, segment_) %>% 
  inner_join(prim$path) %>% select(segment_, object) %>% 
  group_by(segment_)


x <- PATH(columbus)
sc_primitive(x)
all_coordinates <- function(vertex, path_link_vertex) {
  dplyr::inner_join(path_link_vertex, vertex, "vertex_")
}
graph_segments <- function(x, ...) {
  ## assume this is the path_segments .vertex0, .vertex1, path, segment_
  ## so we need another table to link segment to path
  
}
segment_split_path <- function(allcoords) {
  dplyr::bind_rows(lapply(split(allcoords, allcoords$path),
                                              function(x) path_to_segment(x$vertex_, x$path[1L])))
}
path_segments <- function(x, ...) {
  allcoords <- all_coordinates(x$vertex, x$path_link_vertex)
  ## what if we had points
  if (nrow(x$path_link_vertex) == nrow(distinct_(x$path_link_vertex, "path"))) {
    return(dplyr::transmute_(allcoords, .vertex0 = quote(vertex_), path = quote("path")))
  }
  ## this is a subset of RTriangle::pslg (because the original target was RTriangle::triangulate)
  #pstraight_lgraph <- list(P = as.matrix(v[, c("x_", "y_")]),
  ## only need S for 1D
  segment_pairs <- segment_split_path(allcoords)
  segment_pairs[["segment_"]] <- sc_uid(n = nrow(segment_pairs))     
  segment_pairs
}



