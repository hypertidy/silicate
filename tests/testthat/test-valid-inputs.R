context("test-valid-inputs.R")

pbroke <- p <- PATH(minimal_mesh)
test_that("we stop on bad inputs", {
  pbroke$vertex$x_[10] <- NA
  expect_error(earcut(pbroke), "missing values in x")
  
  pbroke$vertex <- pbroke$vertex[1:2, ]
  expect_error(earcut(pbroke), "need at least 3 coordinates")
  #expect_output(earcut(minimal_mesh$geom[1]), "Column 1 must be named")
#  expect_error(earcut(minimal_mesh$geom[[1]]), "Evaluation error: invalid 'times' argument.")
  
})

test_that("we work on good inputs", {
  expect_silent(earcut(minimal_mesh$geom))
  
})