context("test-colours")



test_that("plots have known output", {
  #library(vdiffr)
  fun_SC <- function() plot(SC(minimal_line))
  fun_PATH <- function() plot(PATH(minimal_line))
  fun_SC0 <- function() plot(SC0(minimal_line))

  fun_SC_p <- function() plot(SC(minimal_mesh))
  fun_PATH_p <- function() plot(PATH(minimal_mesh))
  fun_TRI_p <- function() plot(TRI(minimal_mesh))
  fun_SC0_p <- function() plot(SC0(minimal_mesh))

  set.seed(2)
  # expect_doppelganger("SC line plot", fun_SC)
  # expect_doppelganger("PATH line plot", fun_PATH)
  # expect_doppelganger("SC0 line plot", fun_SC0)
  #
  # expect_doppelganger("SC poly plot", fun_SC_p)
  # expect_doppelganger("PATH poly plot", fun_PATH_p)
  # expect_doppelganger("TRI poly plot", fun_TRI_p)
  # expect_doppelganger("SC0 poly plot", fun_SC0_p)
  #

  ## warns on CRAN
#  expect_silent( fun_SC())
#  expect_silent( fun_SC0())
#  expect_silent( fun_SC_p())
#  expect_silent(fun_SC0_p())



  expect_silent( fun_PATH())

  expect_silent(fun_PATH_p())
  expect_silent( fun_TRI_p())

#  expect_silent(plot(PATH0(minimal_mesh)))
#  expect_silent(plot(TRI0(minimal_mesh)))

})
