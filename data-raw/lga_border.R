library(sf)

lga <- ozmaps::abs_lga
oz <- ozmaps::abs_ste
nsw <- unlist(lapply(st_intersects(lga, oz %>% dplyr::filter(NAME == "New South Wales")), length) > 0)
vic <- unlist(lapply(st_intersects(lga, oz %>% dplyr::filter(NAME == "Victoria")), length) > 0)
nsw_vic <- lga[nsw & vic, ]
## if your centroid is in the state, you are in
nsw2 <- unlist(lapply(st_intersects(st_centroid(nsw_vic), oz %>% dplyr::filter(NAME == "New South Wales")), length) > 0)
vic2 <- unlist(lapply(st_intersects(st_centroid(nsw_vic), oz %>% dplyr::filter(NAME == "Victoria")), length) > 0)
# plot(nsw_vic[nsw2 | vic2, ], reset = F)
# plot(oz, add = T, col = "transparent")
sfx <- nsw_vic[nsw2 | vic2, ]


sfx <- sfx[st_coordinates(st_centroid(sfx))[,1] < 142.3, ]
sf::st_write(sfx, "inst/extdata/lga_border.gpkg")

