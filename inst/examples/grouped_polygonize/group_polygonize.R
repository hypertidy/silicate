library(raster)
library(dplyr)
library(sf)
fa_st_polygon <- function(x) {
  structure(x, class = c("XY", "POLYGON", "sfg"))
}
fa_st_sfc <- function(x, crs = NA_crs_, bbox) {
  structure(x, class = c("sfc_POLYGON", "sfc"), precision = 0,
            bbox = bbox, crs = crs, n_empty = 0)
}

#m <- matrix(1:12, 3, 4) %/% 4
m <- volcano %/% 20
r <- setExtent(raster(m), extent(0, nrow(m), 0, ncol(m)))


## this is a fair stab, but holes won't work (end up with overlapping polygons)
polygonize_grouped <- function(r) {
  ## quadmesh, all corner coordinates indexed in quads
  qm <- quadmesh::quadmesh(r, z = NULL)
  ## qm coordinates in upright form
  V <- t(qm$vb[1:2, ])
  ## link table between quads and V
  qXv <- tibble::tibble(vertex_ = as.integer(qm$ib),
                        quad_ = rep(seq_len(ncol(qm$ib)), each = 4L))
  qXv$value <- rep(values(r), each = 4)

  ## visit each set of pixel-value groups in turn
  uvalues <- unique(qXv$value)
  list_sf <- vector("list", length(uvalues))
  ## path to segments
  p2s <- function(x) head(matrix(x,  ncol = 2, nrow = length(x) + 1), -1L)
  prj <- projection(r)
  for (igroup in seq_along(uvalues)) {
    x0 <- dplyr::filter(qXv, value == uvalues[igroup])
    edges <- do.call(rbind, lapply(split(x0$vertex_, x0$quad_), p2s))
    edges <- cbind(pmin(edges[,1], edges[,2]), pmax(edges[,1], edges[,2]))

    ## detect all edges that aren't on a boundary, they only occur once
    edges <- edges %>% tibble::as_tibble() %>%
      setNames(c(".vx0", ".vx1")) %>% dplyr::mutate(edgelab = paste(.vx0, .vx1, sep = "-")) %>%
      dplyr::group_by(edgelab) %>% dplyr::mutate(count = n()) %>% dplyr::ungroup() %>%
      dplyr::filter(count < 2) %>% arrange(.vx0, .vx1)

    l <- purrr::transpose(edges[c(".vx0", ".vx1")]) %>%
      purrr::map(~st_linestring(V[unlist(.x), ])) %>% st_sfc()

#browser()
    ## process the holes out, given assumptions re GEOS return logic
    g <- st_polygonize(st_union(l))
    gpolygon <- unlist(g, recursive = FALSE) ## lapply(g, function(x) st_polygon(unclass(x)))
    gatomic <- unlist(gpolygon, recursive = FALSE)
    ## make a map of where we are going
    gmap <- gibble::gibble(gpolygon)   %>%
      group_by(object) %>%
      mutate(hole = cumsum(subobject) > 1) %>% ungroup() %>%
      mutate(id = "")
    ## loop over paths, digest in assumed clockwise order
    for (i in seq_len(nrow(gmap))) {
      mat <- gatomic[[i]]
      ## pretty sure GEOS will reliably return holes reversed compared to islands
      if (gmap$hole[i]) mat <- mat[rev(seq_len(nrow(mat))), ]
      gmap$id[[i]] <- digest::digest(mat)
    }
    ## gmap where not-hole, but duplicate is to be removed
    bad <- (!gmap$hole) & duplicated(gmap$id)
    if (any(bad)) gpolygon <- gpolygon[-which(bad[!gmap$hole])]

  list_sf[[igroup]] <- st_multipolygon(gpolygon)
    #print(igroup)
  }
#browser()
  sf::st_sf(value = uvalues, geometry = do.call(st_sfc, list_sf)) %>% group_by(value) %>% summarize() %>% st_cast("MULTIPOLYGON")
}

x <- polygonize_grouped(r)

y <- st_as_sf(raster::rasterToPolygons(r, dissolve = TRUE)) %>%
  dplyr::rename(value = layer)
plot(x); plot(y)





