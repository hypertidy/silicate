context("test-generic-data.R")
x <- c(.1, .1, .9, .9, NA, .2, .2, .8, .8)
y <- c(.1, .9, .9, .1, NA, .2, .8, .8, .2)

poly <- list(x = x, y = y)

napos <- c(5, 15, 23, 80)
l <- setNames(lapply(1:5, function(ignore) {x <- rnorm(100); x[napos] <- NA; x}), c("x", "y", "z", "a", "b"))

test_that("generic forms are understood", {
  expect_equal(sc_coord(poly), na.omit(tibble::tibble(x = x, y = y)))
  expect_equal(sc_path(poly), tibble::tibble(nrow = c(4L, 4L)))

  sc_coord(l) %>%  expect_s3_class("tbl_df") %>% expect_length(5L)
  sc_path(l) %>%  expect_s3_class("tbl_df") %>% expect_length(1L)

})


