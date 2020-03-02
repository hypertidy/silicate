context("test-arc-tests.R")

test_that("ARC for non polygons is a warnable offence", {
  expect_silent(ARC(minimal_mesh))
  expect_warning(ARC(minimal_line), "ARC is not well-defined unless used on polygon layers")
})

