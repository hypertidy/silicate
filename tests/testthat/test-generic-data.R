context("test-generic-data.R")
x <- c(.1, .1, .9, .9, NA, .2, .2, .8, .8)
y <- c(.1, .9, .9, .1, NA, .2, .8, .8, .2)

poly <- list(x = x, y = y)

napos <- c(5, 15, 23, 80)
l <- setNames(lapply(1:5, function(ignore) {x <- rnorm(100); x[napos] <- NA; x}), c("x", "y", "z", "a", "b"))

m <- cbind(x, y)
d <- data.frame(a = x[1:4], y = y[1:4])
d1 <- data.frame(x = x, y = y)
test_that("generic forms are understood", {
  expect_equal(sc_coord(poly), na.omit(tibble::tibble(x = x, y = y)))
  expect_equal(sc_path(poly), tibble::tibble(nrow = c(4L, 4L)))

  sc_coord(l) %>%  expect_s3_class("tbl_df") %>% expect_length(5L)
  sc_path(l) %>%  expect_s3_class("tbl_df") %>% expect_length(1L)

  ## no separators
  sc_coord(d) %>%  expect_s3_class("tbl_df") %>% expect_length(2L)
  expect_error(sc_path(d))
  })


