context("test-valid-inputs.R")

pbroke <- p <- PATH(minimal_mesh)
test_that("we stop on bad inputs", {
  pbroke$vertex$x_[10] <- NA
  expect_error(TRI(pbroke), "missing values in x_")
  
  pbroke$vertex <- pbroke$vertex[1:2, ]
  expect_error(TRI(pbroke), "need at least 3 coordinates")
  #expect_output(earcut(minimal_mesh$geom[1]), "Column 1 must be named")
#  expect_error(earcut(minimal_mesh$geom[[1]]), "Evaluation error: invalid 'times' argument.")
  
})

