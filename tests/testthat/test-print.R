context("test-trip")
test_that("print works", {
  expect_output(print(PATH(minimal_mesh)))
  expect_output(print(SC(minimal_mesh)))
  expect_output(print(ARC(minimal_mesh)))
  expect_output(print(TRI(minimal_mesh)))

  expect_output(print(PATH0(minimal_mesh)))
  expect_output(print(SC0(minimal_mesh)))
  expect_error(print(ARC0(minimal_mesh)))
  expect_output(print(TRI0(minimal_mesh)))

})
