library(raster)
m <- matrix(1:12, 3, 4) %/% 4
m <- volcano %/% 20

r <- setExtent(raster(m), extent(0, nrow(m), 0, ncol(m)))
#r <- anglr::gebco1 %/% 1000
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
ps <- RTriangle::pslg(P = as.matrix(uvertex[c("x_", "y_")]))

for (igroup in seq_along(uvalues)) {
  x0 <- dplyr::filter(qXv, value == uvalues[igroup])
  edges <- do.call(rbind, lapply(split(x0$vertex_, x0$quad_), f))
  ## detect all edges that aren't on a boundary, they only occur once
  ## no need to sort as all edges are in the same order, so identity is fine
  #edges <- t(apply(edges, 1, sort))
  edges <- edges %>% tibble::as_tibble() %>%
    setNames(c(".vx0", ".vx1")) %>% dplyr::mutate(edgelab = paste(.vx0, .vx1, sep = "-")) %>%
    dplyr::group_by(edgelab) %>% dplyr::mutate(count = n()) %>% dplyr::ungroup() %>%
    dplyr::filter(count < 2)

ps$S <- as.matrix(edges[c(".vx0", ".vx1")])
  tr <- RTriangle::triangulate(ps)
  TT <- split(t(cbind(tr$T, tr$T[,1])), rep(seq_len(nrow(tr$T)), each = 4L))
 bb <- c(range(ps$P[c(tr$T),1]), range(ps$P[c(tr$T),2]))[c(1, 3, 2, 4)]

  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
bb <- structure(bb, crs = NA_crs_, class = "bbox")
  #list_sf[[igroup]] <- sf::st_union(sf::st_sfc(apply(tr$T, 1, function(i) fa_st_polygon(list(ps$P[c(i, i[1]), ]))), crs = projection(r)))
  list_sf[[igroup]] <- sf::st_union(fa_st_sfc(lapply(TT,
      function(i) fa_st_polygon(list(ps$P[i, ]))), crs = NA_crs_, bbox = bb))

  print(igroup)
}


x <- sf::st_sf(value = uvalues, geometry = sf::st_cast(do.call(c, list_sf)))

plot(x)
