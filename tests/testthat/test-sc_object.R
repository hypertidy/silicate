context("test-sc_object")

test_that("sfc object works", {
  expect_equal(sc_coord(minimal_mesh$geom),
               sc_coord(minimal_mesh))

  expect_equal(sc_coord(minimal_mesh$geom),
               sc_coord(PATH(minimal_mesh)))

  expect_equal(nrow(sc_object(minimal_mesh$geom)),
               nrow(sc_object(minimal_mesh)))

  expect_named(sc_object(minimal_mesh$geom), "object_")
  expect_named(sc_object(minimal_mesh), c("a"))
  expect_equal(nrow(sc_object(minimal_mesh$geom)),
               nrow(sc_object(PATH(minimal_mesh))))

  expect_named(sc_object(PATH(minimal_mesh)), c("a", "object_"))


#  expect_equal(sc_object)
})
