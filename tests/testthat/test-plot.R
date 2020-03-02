context("test-plot")

test_that("plot works", {
  skip_on_cran()  ## we get warnings
  set.seed(8)
  #expect_equal(sc_colour_values(c(2.1, 8.3, -2, 3)), c("#070300", "#540011", "#190E00", "#001356"))
  s0 <- SC0(minimal_mesh)
  s0$object$color_ <- c("firebrick", "dodgerblue")
  expect_silent(plot(s0, lwd = c(2, 7)))

  expect_silent(plot(PATH(minimal_mesh)))
  expect_silent(plot(ARC(minimal_mesh)))
})
