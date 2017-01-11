context("branch-decomposition")
library(sf)

nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
data("sfzoo")
data("sfgc")
test_that("raw geometry decomposition works", {
  dplyr::bind_rows(lapply(sfzoo, sc_branch)) %>% 
  expect_s3_class("tbl_df") %>% 
    expect_named(c("ncoords_", "branch_", "island_"))
})

inner_cascade <- function(x) {
  tabnames <- sc:::join_ramp(x)
  tab <- x[[tabnames[1]]]
  for (ni in tabnames[-1L]) tab <- dplyr::inner_join(tab, x[[ni]])
  tab
}

test_that("geometrycollection decomposition works", {
  dplyr::bind_rows(lapply(sfgc, sc_branch)) %>% 
  expect_s3_class("tbl_df") %>% 
    expect_named(c("ncoords_", "branch_", "island_"))
})


test_that("sf decomposition works", {
  BRANCH(nc) %>% 
    expect_s3_class("BRANCH") %>% 
    expect_named(c("object", "branch", "vertex", "branch_link_vertex"))
})

test_that("joins are valid", {
  BRANCH(nc) %>% inner_cascade() %>% 
    expect_s3_class("tbl_df")
})