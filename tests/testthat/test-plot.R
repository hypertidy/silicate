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


xpoints <- structure(list(a = 1L, geom = structure(list(structure(c(0, 0,
                                                                    0.75, 1, 0.5, 0.8, 0.69, 0, 0.2, 0.5, 0.5, 0.3, 0.2, 0.2, 0,
                                                                    1, 1, 0.8, 0.7, 0.6, 0, 0, 0.2, 0.2, 0.4, 0.6, 0.4, 0.2), .Dim = c(14L,
                                                                                                                                       2L), .Dimnames = list(NULL, c("x", "y")), class = c("XY", "MULTIPOINT",
                                                                                                                                                                                           "sfg"))), precision = 0, bbox = structure(c(xmin = 0, ymin = 0,
                                                                                                                                                                                                                                       xmax = 1, ymax = 1), class = "bbox"), crs = structure(list(input = NA_character_,
                                                                                                                                                                                                                                                                                                  wkt = NA_character_), class = "crs"), n_empty = 0L, class = c("sfc_MULTIPOINT",
                                                                                                                                                                                                                                                                                                                                                                "sfc"))), row.names = 1L, class = c("sf", "data.frame"), sf_column = "geom", agr = structure(c(a = NA_integer_), .Label = c("constant",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "aggregate", "identity"), class = "factor"))
test_that("weird plots", {
expect_warning(plot(PATH(xpoints)), "degenerate")
  expect_silent(plot(PATH0(minimal_mesh)))
})
