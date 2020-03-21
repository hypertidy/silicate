# I think we should use nested data frames, as an analogue to sf but that is
# able to utilize the tidyr nest/unnest facilities. It's also a straightforward
# switch from data.frame to list for leaflet and so on. It makes no sense to me
# that POINT is a vector, that only causes problems and requires exceptional
# handling compared to all other cases. But sf is not designed for developers,
# it's designed. I wish we had an agreed central standard for hierarchical data,
# but there seems to be no appetite for it. It will come to us from the
# tidyverse with separated tables that allow linking between structures, without
# needlessly copying repeated elements.
# 
# I plan to write these sf-geometry analogues, since then it's a simple step to
# the tidyverse nested ways. Turning nested dataframes inside out also provides

library(sf)
library(scsf)
x <- minimal_mesh
## sc_coord and sc_path are bound together
## coord is all instances of vertices
## each path row has the vertex count, effectively an sequential index into coord
sc_coord(x)
sc_path(x)

## sp, sf, spatstat objects are PATHs

## icosa, rgl, deldir, RTriangle, objects are not PATHs

# the pathway to PATH is

GENERAL: 
  sc_object(x)
  sc_path(x, ids= <object_id>)
  sc_coord(x)

PRIM-1D: find cycles, express as winded paths

## the pathway to PRIMITIVE is

PATH: create segments/edges

PRIM-1D: create PRIM-2D

mesh: express as PRIM-1D (hexagons, segments, quads, triangulations)


sc_segment are the PRIM-1D elements
sc_vertex  the unique vertices


segment and/or edge need to differentiate oriented and non-oriented



library(icosa)
x <- trigrid()

sc_segment <- function(x, ...) {
  UseMethod("sc_segment")
}
sc_segment.trigrid <- function(x, ...) {
  ## these are non-oriented
  setNames(tibble::as_tibble(x@edges), c(".vertex0", ".vertex1"))
}
sc_vertex <- function(x, ...) {
  UseMethod("sc_vertex")
}
sc_vertex.trigrid <- function(x, ...) {
  vids <- rownames(x@vertices)
  dplyr::mutate(setNames(tibble::as_tibble(x@vertices), c("x_", "y_", "z_")), vertex_ = vids)
}
sc_segment(x)
sc_vertex(x)

PRIMITIVE.trigrid <- function(x, ...) {
  
}
plot3d(x)
