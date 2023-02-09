test_that("no new tibble messages", {
  skip_if_not_installed("sp")
  library(sp)
  xsp <- .nc_sp ## needed to flush sp messages (or something)
  expect_silent(x <- SC0(xsp))
})
