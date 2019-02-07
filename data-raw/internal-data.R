library(sf)
minimal_line <- sf::st_sf(a = 1:3,
                          geometry = sf::st_sfc(
                            sf::st_linestring(cbind(c(0, 0.5, 1), c(0,0, 0))),
                            sf::st_linestring(cbind(c(0.5, 1, 1), c(0, 0, 0.5))),
                            sf::st_linestring(cbind(c(0, 1), c(1, 1)))))






raw_chars <- c(LETTERS, letters, 0:9)
viridis_cols <- viridis::viridis(256)
usethis::use_data(viridis_cols,raw_chars,minimal_line, internal = TRUE)
