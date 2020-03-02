library(raster)
 

r <- (raster(matrix(seq_len(12*15), 12)) %/% 40) * raster(matrix(seq_len(12*15), 12, byrow = TRUE)) %/% 40
library(dplyr)
library(sf)
polymesh <- st_cast(spex::polygonize(r) %>% group_by(layer) %>% summarize(), 
                    "MULTIPOLYGON")

class(polymesh$geometry) <- c("sfc_MULTIPOLYGON", "sfc" , "list")
usethis::use_data(polymesh, overwrite = TRUE)



# 
# library(dplyr)
# library(silicate)
# x <- PATH(polymesh)
# unique_edges <- dplyr::distinct(silicate:::sc_segment_base(x$path_link_vertex), edge, .keep_all = TRUE)
# vertex <- x$vertex
# nodes <- silicate:::sc_node_base(unique_edges, vertex)
# 
# arcs <- silicate:::sc_arc_base(x$path_link_vertex, nodes) 
# distinct(arcs, arc)
# 
# 
# sarcs <- split(arcs, arcs$arc)
# for (i in seq_along(sarcs)) {
#   plot(sf::st_cast(st_geometry(polymesh), "MULTILINESTRING"), col = "black")
#   lines(sarcs[[i]] %>% inner_join(x$vertex) %>% dplyr::select(x_, y_), col = "firebrick", lwd = 2)
#   scan("", 1)
# }
