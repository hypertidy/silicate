## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)

## ----minimal_mesh--------------------------------------------------------
library(silicate)

plot(matrix(attr(minimal_mesh$geom, "bbox"), 2, byrow = TRUE), type = "n", xlab = "", ylab = "")
rbind_na <- function(x) head(do.call(rbind, unlist(lapply(x, function(a) rbind(a, NA)), recursive = F)), -1)
cols <- sc_colours(nrow(minimal_mesh))
junk <- lapply(seq_along(minimal_mesh$geom), function(y) polypath(rbind_na(minimal_mesh$geom[[y]]), col = cols[y] ))



## ----sc------------------------------------------------------------------
x <- SC(minimal_mesh)
names(x)
x$edge
print(x)
plot(x)
text(x$vertex[c("x_", "y_")], label = x$vertex$vertex_)

## ----PATH----------------------------------------------------------------
x <- PATH(minimal_mesh)
names(x)

## ----reduce-path---------------------------------------------------------

purrr::reduce(x[c("object", "path", "path_link_vertex", "vertex")], dplyr::inner_join)

## ----arc-node------------------------------------------------------------
library(dplyr)
arc <- ARC(minimal_mesh)
nodes <- sc_node(arc)

plot(arc)
inner_join(nodes, arc$vertex) %>% dplyr::select(x_, y_) %>% points(pch = "N")


## ----TRI-----------------------------------------------------------------
tri <- TRI(minimal_mesh)
plot(tri)

## ------------------------------------------------------------------------
#system.time(sf::st_triangulate(inlandwaters))
#   user  system elapsed 
#  4.699   0.125   4.823 
#system.time(sfdct::ct_triangulate(inlandwaters))
#  user  system elapsed 
# 15.476   0.225  15.460 
system.time(tri <- TRI(inlandwaters))

## ------------------------------------------------------------------------
plot(tri)

plot(NA, xlim = c(625000, 1060000), ylim = c(-1350000,  -550000))
plot(tri, add = TRUE)

## ----sfdct,eval=FALSE----------------------------------------------------
#  #plot(sfdct::ct_triangulate(minimal_mesh))

