context("path-decomposition")
#library(sf)
#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
data("sfzoo")
data("sfgc")
test_that("raw geometry decomposition works", {
  dplyr::bind_rows(lapply(sfzoo, sc_path)) %>% 
  expect_s3_class("tbl_df") %>% 
    expect_named(c("nrow", "ncol", "type", "path", "subobject"))
})
#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
inner_cascade <- function(x) {
  tabnames <- join_ramp(x)
  tab <- x[[tabnames[1]]]
  for (ni in tabnames[-1L]) tab <- dplyr::inner_join(tab, x[[ni]])
  tab
}
#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
test_that("geometrycollection decomposition works", {
  dplyr::bind_rows(lapply(sfgc, sc_path)) %>% 
  expect_s3_class("tbl_df") %>% 
    expect_named(c("nrow", "ncol", "type", "path", "subobject"))
})

#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
test_that("sf decomposition works", {
  PATH(minimal_mesh) %>% 
    expect_s3_class("PATH") %>% 
    expect_named(c("object", "path", "vertex", "path_link_vertex"))
})
#nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
test_that("joins are valid", {
  PATH(minimal_mesh) %>% inner_cascade() %>% 
    expect_s3_class("tbl_df")
})

