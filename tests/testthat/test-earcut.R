context("test-earcut")

test_that("earcut works", {
  idx<- c(9L, 8L, 7L, 5L, 4L, 3L, 2L, 1L, 12L, 10L, 9L, 7L, 7L, 12L,
         1L, 5L, 3L, 2L, 2L, 12L, 11L, 5L, 2L, 11L, 6L, 5L, 11L, 6L, 11L,
         10L, 1L, 6L, 10L, 6L, 7L, 14L, 14L, 13L, 6L)
 earcut_PATH(PATH(minimal_mesh), equals(idx))
})
