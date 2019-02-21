context("test-sc_coord")

test_that("weird case works", {
  expect_equal(sc_coord(list(coord = tibble::tibble(xx = 2, yy = 1))), tibble::tibble(xx = 2, yy = 1))

  expect_equal(sc_coord(cbind(xx = 1, yy = 2)), tibble::tibble(xx = 1, yy = 2))
})
