context("test-colours")

test_that("colours works", {
  set.seed(2)
  expect_equal(sc_colours(23), c("#CB00C6", "#030003", "#0098DE", "#260005", "#222600", "#004C33",
                                 "#4A8300", "#00491F", "#5B001F", "#1D0D00", "#00012A", "#C5D700",
                                 "#221A00", "#00F14E", "#5A00A0", "#0091CF", "#AA0096", "#130059",
                                 "#002A22", "#F50053", "#262600", "#A1FA00", "#00467D"))

  expect_equal(sc_colours(6, viridis = TRUE), c("#440154", "#414487", "#2A788E", "#22A883", "#7AD151", "#FDE725"
  ))


  expect_silent(plot(SC(minimal_line)))
  expect_silent(plot(PATH(minimal_mesh)))
  expect_silent(plot(TRI(minimal_mesh)))
  expect_silent(plot(SC0(minimal_line)))
#  expect_silent(plot(PATH0(minimal_mesh)))
#  expect_silent(plot(TRI0(minimal_mesh)))

})
