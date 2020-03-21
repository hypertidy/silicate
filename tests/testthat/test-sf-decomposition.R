context("path-decomposition")
data("sfzoo")
data("sfgc")
test_that("raw geometry decomposition works", {
  dplyr::bind_rows(lapply(sfzoo, sc_path)) %>%
  expect_s3_class("tbl_df") %>%
    expect_named(c("nrow", "ncol", "type", "path_", "subobject"))
})
inner_cascade <- function(x) {
  tabnames <- join_ramp(x)
  tab <- x[[tabnames[1]]]
  for (ni in tabnames[-1L]) tab <- dplyr::inner_join(tab, x[[ni]])
  tab
}
test_that("geometrycollection decomposition works", {
  dplyr::bind_rows(lapply(sfgc, sc_path)) %>%
  expect_s3_class("tbl_df") %>%
    expect_named(c("nrow", "ncol", "type", "path_", "subobject"))
  
  expect_silent( sc_path(silicate::sfgc))
})

test_that("sf decomposition works", {
  path <- PATH(minimal_mesh)
  path %>%   expect_s3_class("PATH")
  expect_true(all(c("object", "path", "path_link_vertex", "vertex") %in% names(path)))
})
test_that("joins are valid", {
  PATH(minimal_mesh) %>% inner_cascade() %>%
    expect_s3_class("tbl_df")
})
obj <- polymesh
test_that("object and path names as expected", {
   gibble::gibble(obj) %>% expect_named(c("nrow", "ncol", "type", "subobject", "object"))
   expect_true("layer" %in%                              names(sc_object(obj)))
   expect_true(all(c("arc_", "ncoords_") %in%               names(sc_arc(obj))))
   expect_true(all(c("x_", "y_") %in%                      names(sc_coord(obj))))
   expect_true(all(c(".vx0", ".vx1") %in% names(sc_edge(obj))))
   expect_equal("vertex_",                                       names(sc_node(obj)))
   expect_true(all(c("object_", "path_", "ncoords_") %in%  names(sc_path(obj))))
   expect_true(all(c(".vx0", ".vx1",
                      "segment_", "edge_") %in%    names(sc_segment(obj))))



}  )


test_that("topology basis is sane", {
  expect_identical(PATH0(minimal_mesh)$vertex, SC0(minimal_mesh)$vertex)

  #identical(TRI0(minimal_mesh)$vertex, SC0(minimal_mesh)$vertex)
})

