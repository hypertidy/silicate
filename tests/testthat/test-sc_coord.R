context("test-sc_coord")

test_that("weird case works", {
  expect_equal(sc_coord(list(coord = tibble::tibble(xx = 2, yy = 1))), tibble::tibble(xx = 2, yy = 1))

  expect_equal(sc_coord(cbind(xx = 1, yy = 2)), tibble::tibble(xx = 1, yy = 2))

  x <- silicate::PATH0(minimal_mesh)
  expect_silent(sc_coord(x))  ##issue107

})

test_that("drop sfc etc works", {
  mm <- minimal_mesh
 # class(mm) <- "data.frame"
  class(mm$geom) <- "list"
  expect_equivalent(sc_coord(mm),
                    sc_coord(minimal_mesh))

})
