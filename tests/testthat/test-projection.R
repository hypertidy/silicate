#context("test-projection.R")
#
# obj <- sf::st_set_crs(minimal_mesh, "+proj=lcc")
# test_that("projection meta works", {
#   x <- SC(obj)
#   expect_that(x$meta$proj, equals("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
#
#   x <- ARC(obj)
#   expect_that(x$meta$proj, equals("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
#
#   x <- silicate::TRI(obj)
#   expect_that(x$meta$proj, equals("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
#
#  x <- silicate::PATH(obj)
#  expect_that(x$meta$proj, equals("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
#
#    })
