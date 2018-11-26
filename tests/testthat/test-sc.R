context("test-sc-model")

sc <- SC(minimal_mesh)
test_that("SC works", {
  sc <- expect_s3_class(sc, "SC") %>%
    expect_named(c("object", "object_link_edge", "edge", "vertex", "meta"))
})

test_that("SC verb methods work", {
  #expect_s3_class(sc_arc(sc), "tbl_df")
  expect_error(sc_arc(sc), "ARC not yet implemented for SC")
  expect_s3_class(sc_coord(sc), "tbl_df")
  expect_s3_class(sc_edge(sc), "tbl_df")

  expect_s3_class(sc_object(sc), "tbl_df")
  expect_s3_class(sc_edge(sc), "tbl_df")
  #expect_s3_class(sc_path(sc), "tbl_df")
  expect_error(sc_path(sc), "sc_path not yet supported for SC")

  expect_s3_class(sc_segment(sc), "tbl_df")
  expect_s3_class(sc_vertex(sc), "tbl_df")

  expect_s3_class(sc_end(sc), "tbl_df")
  expect_s3_class(sc_start(sc), "tbl_df")

}
)
