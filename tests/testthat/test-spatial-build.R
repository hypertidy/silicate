context("test-spatial-build")

test_that("building sf works", {
  gib <- gibble(minimal_mesh)
  cds <- sc_coord(minimal_mesh)
  expect_s3_class(build_sf(PATH(minimal_mesh)), "sf")
})
