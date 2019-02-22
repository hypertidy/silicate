context("test-colours")

test_that("colours works", {
  set.seed(2)
  expect_equal(sc_colours(23), c("#CB00C6", "#030003", "#0098DE", "#260005", "#222600", "#004C33",
                                 "#4A8300", "#00491F", "#5B001F", "#1D0D00", "#00012A", "#C5D700",
                                 "#221A00", "#00F14E", "#5A00A0", "#0091CF", "#AA0096", "#130059",
                                 "#002A22", "#F50053", "#262600", "#A1FA00", "#00467D"))

  expect_equal(sc_colours(6, viridis = TRUE), c("#440154", "#414487", "#2A788E", "#22A883", "#7AD151", "#FDE725"
  ))

})

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
  expect_silent( fun_SC())
  expect_silent( fun_PATH())
  expect_silent( fun_SC0())

  expect_silent( fun_SC_p())
  expect_silent(fun_PATH_p())
  expect_silent( fun_TRI_p())
  expect_silent(fun_SC0_p())

#  expect_silent(plot(PATH0(minimal_mesh)))
#  expect_silent(plot(TRI0(minimal_mesh)))

})
