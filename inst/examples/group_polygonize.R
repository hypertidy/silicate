library(raster)
m <- matrix(1:12, 3, 4) %/% 4
m <- volcano %/% 20
library(dplyr)
r <- setExtent(raster(m), extent(0, nrow(m), 0, ncol(m)))
r <- anglr::gebco1 %/% 1000
#plot(r)

library(anglr)
qm <- QUAD(r)
uvertex <- anglr:::edges0(r) %>% tibble::as_tibble() %>% setNames(c("x_", "y_"))
## vertex_ must be integer for the RTriangle stuff
qXv <- anglr:::get_qXv(qm) %>% dplyr::mutate(vertex_ = as.integer(vertex_))
qXv$value <- rep(qm$quad$value, each = 4)

## visit each set of pixel-value groups in turn
uvalues <- unique(qXv$value)

list_sf <- vector("list", length(uvalues))
fa_st_polygon <- function(x) {
  structure(x, class = c("XY", "POLYGON", "sfg"))
}
fa_st_sfc <- function(x, crs = NA_crs_, bbox) {

  structure(x, class = c("sfc_POLYGON", "sfc"), precision = 0,
            bbox = bbox, crs = crs, n_empty = 0)
}
f <- function(x) head(matrix(x,  ncol = 2, nrow = length(x) + 1), -1L)

#c1 <- function(x) c(x, x[1L])
prj <- projection(r)
V <- as.matrix(uvertex[c("x_", "y_")])
ps <- RTriangle::pslg(P = V)
library(sf)
for (igroup in seq_along(uvalues)) {
  x0 <- dplyr::filter(qXv, value == uvalues[igroup])
  edges <- do.call(rbind, lapply(split(x0$vertex_, x0$quad_), f))
  edges <- cbind(pmin(edges[,1], edges[,2]), pmax(edges[,1], edges[,2]))
  #edges <- edges[!duplicated(edges), ]

  ## detect all edges that aren't on a boundary, they only occur once
  edges <- edges %>% tibble::as_tibble() %>%
    setNames(c(".vx0", ".vx1")) %>% dplyr::mutate(edgelab = paste(.vx0, .vx1, sep = "-")) %>%
    dplyr::group_by(edgelab) %>% dplyr::mutate(count = n()) %>% dplyr::ungroup() %>%
    dplyr::filter(count < 2) %>% arrange(.vx0, .vx1)

l <- purrr::transpose(edges[c(".vx0", ".vx1")]) %>%
  purrr::map(~st_linestring(V[unlist(.x), ])) %>% st_sfc()

list_sf[[igroup]] <- st_polygonize(st_union(l))
  print(igroup)
}


x <- sf::st_sf(value = uvalues, geometry = do.call(c, list_sf)) %>% group_by(value) %>% summarize() %>% st_cast("MULTIPOLYGON")

plot(x)
