## .tr and .xyz are internal data
test_that("dimensionality works", {

  
  expect_named(TRI(.xyz)$vertex, c("x_", "y_", "z_", "vertex_"))
  expect_named(TRI0(.xyz)$vertex, c("x_", "y_", "z_"))
  
  expect_named(SC(.xyz)$vertex, c("x_", "y_", "z_", "vertex_"))
  expect_named(SC0(.xyz)$vertex, c("x_", "y_", "z_"))

  expect_named(PATH(.xyz)$vertex, c("x_", "y_", "z_", "vertex_"))
  expect_named(PATH0(.xyz)$vertex, c("x_", "y_", "z_"))

  expect_warning(expect_named(ARC(.xyz)$vertex, c("x_", "y_", "z_", "vertex_")))
  #expect_named(ARC0(.xyz)$vertex, c("x_", "y_", "z_"))
  
.trnames <- c("x_", "y_", "t_", "Wet", "Forage", "vertex_")
.tr0names <- c("x_", "y_", "t_", "Wet", "Forage")
  expect_named(TRI(.tr)$vertex, .trnames)
  expect_named(TRI0(.tr)$vertex, .tr0names)
  
  #these are different (for now, not sure - native names or these? and
  # means we don't keep other attributes at all)
  expect_named(SC(.tr)$vertex, c("x_", "y_", "t_", "vertex_"))
  expect_named(SC0(.tr)$vertex, c("x_", "y_", "t_"))
  
  expect_named(PATH(.tr)$vertex, .trnames)
  expect_named(PATH0(.tr)$vertex, .tr0names)
  
  expect_warning(expect_named(ARC(.tr)$vertex, .trnames))
  #expect_named(ARC0(.tr)$vertex, .tr0names)
  
})
