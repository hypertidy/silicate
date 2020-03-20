context("test-arc-tests.R")

#attr(minimal_mesh$geom, "crs")
test_that("ARC for non polygons is a warnable offence", {
  expect_s3_class(ARC(minimal_mesh), "sc")
  expect_warning(ARC(minimal_line), "ARC is not well-defined unless used on polygon layers")
})

