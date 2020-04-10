context("test-triangles")
pts <- structure(c(5L, 3L, 1L, 4L, 4L, 8L, 6L, 9L), .Dim = c(4L, 2L))
tri <- c(2, 1, 3, 2, 4, 1)
a <- tri_area(pts[tri, ])

test_that("triangle tools works", {
  expect_equal(a, c(6, 3))
  expect_equal(tri_ix(pts[tri, ]), c(3, 1, 2, 6, 4, 5))
  expect_equal(tri_jx(pts[tri, ]), c(1, 2, 3, 4, 5, 6))

})


test_that("triangle cull/visibility works", {
  ## first object has a hole in it
  mintri <- silicate::TRI(silicate::minimal_mesh)

  ## TRI model *does not* have triangles that exist within a hole
  expect_true(sum(mintri$triangle$visible) == dim(mintri$triangle)[1L])
})

test_that("primitives extraction works", {
  tri <- TRI(minimal_mesh)
  tri0 <- TRI0(minimal_mesh)
  expect_named(sc_triangle(tri), c(".vx0", ".vx1", ".vx2", "object_", "visible"))
  expect_named(sc_triangle(tri0), c(".vx0", ".vx1", ".vx2", "path_", "object_"))
  
  expect_silent(plot(tri))
  expect_silent(plot(tri, col = viridis_cols))
  expect_silent(plot(tri0))
  expect_silent(plot(tri0, col = viridis_cols))
  
  
})
