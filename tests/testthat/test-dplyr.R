context("test-dplyr")

test_that("filter works", {
  sc <- SC(minimal_mesh)
  sc0 <- SC0(minimal_mesh)
  expect_equal(filter(sc), sc)
 # expect_equal(filter(sc0), sc0)

  expect_equal(nrow(filter(sc, a== 1)$object), 1L)
#  expect_equal(filter(sc0), sc0)

})
