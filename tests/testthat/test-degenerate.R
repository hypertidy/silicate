context("test-degenerate.R")
library(sf)
#> Linking to GEOS 3.5.1, GDAL 2.2.1, proj.4 4.9.3
library(anglr)
library(raster)
#> Loading required package: sp
my_extent <- st_as_sf(as(extent(c(153.185183093, 153.19443135, -27.705328446, -27.6967222119999)), 
                         "SpatialPolygons"))

test_that("sanity reigns", {
  expect_equal(nrow(my_extent), nrow(silicate::PATH(my_extent)$object))
})
