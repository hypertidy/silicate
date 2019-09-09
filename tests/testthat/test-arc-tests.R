context("test-arc-tests.R")

library (sf)
l1 <- cbind (c(1, 5, 10), c(1, 5, 10)) %>% st_linestring ()
l2 <- cbind (c(1, 5, 10), 5) %>% st_linestring ()
x <- list (l1, l2) %>% st_sfc () %>% st_sf ()
#plot(x$geometry)

library(silicate)
a <- ARC(x)

test_that("ARC for non polygons is a warnable offence", {
  expect_silent(ARC(minimal_mesh))
  expect_warning(ARC(x), "ARC is not well-defined unless used on polygon layers")
})
sc_to_dodgr <- function(x, ...) {
  UseMethod("sc_to_dodgr")
}
# sc_to_dodgr.SC <- function(x, ...) {
#   edge <- sc_edge(x)
#   graph <- structure(tibble::tibble(id = seq_len(nrow(edge)),
#                  from = match(edge[[".vertex0"]], x$vertex[["vertex_"]]),
#                  to = match(edge[[".vertex1"]], x$vertex[["vertex_"]]),
#                  d = 1, w = 1),
#             class = c("data.frame", "dodgr_streetnet"))
#   graph_contracted <- dodgr:::rcpp_contract_graph(graph, NULL)
#   graph_contracted
# }
## not yet
# test_that("graph contraction works", {
#   expect_equal(nrow(sc_arc(a)), 4L)
# })

