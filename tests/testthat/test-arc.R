context("test-arc-tests.R")

#attr(minimal_mesh$geom, "crs")
test_that("ARC for non polygons is a warnable offence", {
  arc <- ARC(minimal_mesh)
  expect_s3_class(arc, "sc")
  expect_warning(ARC(minimal_line), "ARC is not well-defined unless used on polygon layers")
  
  expect_named(sc_coord(arc), c("x_", "y_"))
  expect_named(sc_path(arc), c("ncol", "type", "subobject", "object_", "path_", "ncoords_"
  ))
})

