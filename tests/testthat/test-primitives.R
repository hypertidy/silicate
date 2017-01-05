context("primitives")


library(maptools)
data(wrld_simpl)

library(sf)
example(st_read)

x <- dp_triangulate_sf(st_as_sf(wrld_simpl))
plot(x)

gg <- st_read("inlandwaters.gpkg")
system.time(x <- dp_triangulate_sf(gg[6, ]))

## rangl is faster
system.time(rangl(gg[6, ]))

test_that("primitives 0D", {
  error("no tests!")
})
test_that("primitives 1D", {
  error("no tests!")
})

test_that("primitives 2D", {
  error("no tests!")
})
test_that("primitives 3D", {
  error("no tests!")
})
