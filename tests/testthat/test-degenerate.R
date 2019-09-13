context("test-degenerate.R")
gm <- tibble::tibble(nrow = 5L, ncol = 2L, type = "POLYGON", object_ = 1L)
cd <- tibble::as_tibble(list(x_ = c(153.185183093, 153.185183093, 153.19443135,
                            153.19443135, 153.185183093),
                     y_ = c(-27.705328446, -27.6967222119999, -27.6967222119999, -27.705328446,
                            -27.705328446)))
my_extent <- structure(list(geometry = raw_build_sfc(gm, cd)),
                       class = c("sf", "data.frame"), row.names  = "1", sf_column = "geometry", agr = factor(c("constant", "aggregate", "identity")))

test_that("sanity reigns", {
  expect_equal(nrow(my_extent), nrow(silicate::PATH(my_extent)$object))
})
