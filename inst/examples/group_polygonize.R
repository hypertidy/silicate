library(raster)
m <- matrix(1:12, 3, 4) %/% 4
m <- volcano %/% 20

r <- setExtent(raster(m), extent(0, nrow(m), 0, ncol(m)))
r <- anglr::gebco1 %/% 1000
#plot(r)

library(anglr)
qm <- QUAD(r)
tri <- TRI(qm)
tri$triangle$value <- rep(qm$quad$value, each = 2)

## visit each set of pixel-value groups in turn
uvalues <- unique(tri$triangle$value)

list_sf <- vector("list", length(uvalues))
for (igroup in seq_along(uvalues)) {
x0 <- dplyr::filter(tri$triangle, value == uvalues[igroup])
edges <- rbind(as.matrix(x0[c(".vertex0", ".vertex1")]),
               as.matrix(x0[c(".vertex1", ".vertex2")]),
               as.matrix(x0[c(".vertex2", ".vertex0")]))
## detect all edges that aren't on a boundary, they only occur once
edges <- t(apply(edges, 1, sort)) %>% tibble::as_tibble() %>%
  setNames(c(".vx0", ".vx1")) %>% dplyr::mutate(edgelab = paste(.vx0, .vx1, sep = "-")) %>%
  dplyr::group_by(edgelab) %>% dplyr::mutate(count = n()) %>% dplyr::ungroup() %>%
  dplyr::filter(count < 2)
## remove edge groups that were repeated
vertex <- tibble::tibble(vertex_ = as.vector(t(as.matrix(edges[c(".vx0", ".vx1")])))) %>% dplyr::distinct() %>%
  dplyr::inner_join(tri$vertex, "vertex_")

v <- as.matrix(vertex[c("x_", "y_")])
ps <- RTriangle::pslg(P = v,
                      S = cbind(match(edges$.vx0, vertex$vertex_),
                                match(edges$.vx1, vertex$vertex_)))
tr <- RTriangle::triangulate(ps)
list_sf[[igroup]] <- sf::st_union(sf::st_sfc(apply(tr$T, 1, function(i) sf::st_polygon(list(v[c(i, i[1]), ]))), crs = projection(r)))
}


x <- sf::st_sf(value = uvalues, geometry = sf::st_cast(do.call(c, list_sf)))

plot(x)
