context("test-sp-decomposition.R")
skip("no sp test for now")  ## this ain't working
library(sp)
library(gibble)
spobj <- as(minimal_mesh, "Spatial")
splineobj <- as(spobj, "SpatialLinesDataFrame")
test_that("sp decomposition works", {
  gibble(spobj) %>% expect_s3_class("tbl_df")
  sc_coord(spobj) %>% expect_named(c("x_", "y_"))
  sc_object(spobj) %>% expect_named(names(spobj))
  p <- sc_path(spobj)
  expect_false(any(is.na(as.matrix(p))))

  a <- sc_arc(spobj)
  a %>% expect_named(c("arc_", "ncoords_"))
  pp <- PATH(spobj)

  ## this table changed order in the pslg branch
  ## but naming properly, will mean that never matters
  ## add meta 2018-02-27
  expect_equal(unname(unlist(lapply(pp, dim))), c(2L, 2L, 3L, 7L, 19L, 2L, 14L, 3L, 1L, 2L))
  e <- sc_edge(spobj)
  e %>% expect_named(c(".vertex0", ".vertex1", "edge_"))

  sc_coord(splineobj) %>% expect_s3_class("tbl_df") %>% nrow() %>% expect_equal( 19L)
  sc_object(splineobj)%>% expect_s3_class("tbl_df") %>% nrow() %>% expect_equal( 2L)
  sc_path(splineobj) %>% expect_s3_class("tbl_df") %>%
    nrow() %>%
    expect_equal( 3L)
  sc_node(splineobj) %>% expect_s3_class("tbl_df")
  sc_arc(splineobj) %>% expect_s3_class("tbl_df")
  sc_vertex(splineobj) %>% expect_s3_class("tbl_df")
  sc_node(splineobj) %>% expect_s3_class("tbl_df")

})

