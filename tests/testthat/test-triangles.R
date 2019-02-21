context("test-triangles")
pts <- structure(c(5L, 3L, 1L, 4L, 4L, 8L, 6L, 9L), .Dim = c(4L, 2L))
tri <- c(2, 1, 3, 2, 4, 1)
a <- tri_area(pts[tri, ])

test_that("triangle tools works", {
  expect_equal(a, c(6, 3))
  expect_equal(tri_ix(pts[tri, ]), c(3, 1, 2, 6, 4, 5))
  expect_equal(tri_jx(pts[tri, ]), c(1, 2, 3, 4, 5, 6))

})
