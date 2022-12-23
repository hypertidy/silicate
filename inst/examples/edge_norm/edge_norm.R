
library(silicate)
library(raster)
library(sf)

## need quadmesh >= 0.2.0.9002 for https://github.com/hypertidy/quadmesh/issues/18
g <- SC0(sf::st_cast(spex::polygonize(raster(matrix(1:12, 3))),  "LINESTRING"))

l2 <- SC0(SpatialLines(list(Lines(list(Line(list(rbind(c(0.2, 0.2), c(0.5, 0.5), c(0.6, 0.8), c(0.9, 0.9), c(0.2, 0.3), c(0, 0))))), "1"))))
plot(g)
plot(l2, add = TRUE, col = 1:5)
#plot(c(l2, g))
all_list <- list(l2, g)
# - collate all end point indices and vertices
all_edge <-  purrr::map(all_list,
               ~dplyr::bind_rows(tidyr::unnest(.x$object["topology_"]), .id = "object"))
# - increment end points by merge-logic (+cumul row count)
for (i in seq_along(all_edge)) {
  if (i > 1) all_edge[[i]] <- all_edge[[i]] %>% dplyr::mutate_if(is.numeric, dplyr::funs(. + nrow(all_edge[[i - 1]])))
}

edge <- dplyr::bind_rows(all_edge, .id = "input_layer")
# - de-duplicate vertices
dvert <- purrr::map_df(all_list, ~.x$vertex) %>% unjoin::unjoin("x_", "y_")

# - map new end point indices
