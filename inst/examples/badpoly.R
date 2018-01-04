library(sf)
bad_poly <- matrix(c(0.5, 0.5,
                     0.4, 5.2,
                     9.8, 1.1,
                     7.6, 8.5,
                     9.5, 8.0,
                     9.4, 6.1,
                     7.4, 0.7,
                     0.5, 0.5), ncol = 2, byrow = TRUE) %>%
  st_polygon(x = list(.)) %>%
  st_sfc() %>%
  st_sf() 
## add nodes and re-polygonize
x <- st_cast(bad_poly, "MULTILINESTRING") %>% 
  st_node() %>% 
  st_polygonize()
st_set_geometry(x, st_sfc(purrr::map(x$geometry, st_multipolygon)))


## doh
plot(st_buffer(bad_poly, dist = 0))

library(sfdct)
plot(st_buffer(ct_triangulate(bad_poly), dist = .5))

