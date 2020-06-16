context("test-arc-tests.R")

#attr(minimal_mesh$geom, "crs")
test_that("ARC for non polygons is a warnable offence", {
  arc <- ARC(minimal_mesh)
  expect_equal(nrow(arc$object), 2L)
  expect_equal(length(unique(ARC(minimal_mesh)$object_link_arc %>% group_by(object_, arc_) %>% dplyr::group_indices())), 5L)

  expect_s3_class(arc, "sc")
  expect_warning(ARC(minimal_line), "ARC is not well-defined unless used on polygon layers")

  expect_named(sc_coord(arc), c("x_", "y_"))
  expect_named(sc_path(arc), c("ncol", "type", "subobject", "object_", "path_", "ncoords_"
  ))
})

