context("builder")


obj <- polymesh
x <- PATH(obj)

## what is PATH good for?

#############################################################
## convert to arc-node
# library(dplyr)
# arcs <- sc_arc(x)
#
# uarcs <- unique(arcs$arc_)
# dige <- character(length(uarcs))
# for (i in seq_along(uarcs)) dige[i] <- digest::digest(sort(arcs$vertex_[arcs$arc_ == uarcs[i]]))
# arcs$uarc <- rep(dige, rle(arcs$arc_)$lengths)
# arcs_one <- arcs %>% distinct(uarc, .keep_all = TRUE) %>%
#   dplyr::select(arc_)



# ## convert the arcs to sf lines
# gm <- arcs %>% inner_join(arcs_one, "arc_") %>%
#   group_by(arc_) %>% tally() %>%
#   rename(path_ = arc_, nrow = n) %>%
#   mutate(type = "LINESTRING", ncol = 2, object_ = row_number())

## see how arc needs to be arranged, because of the group/tally for the gibble geom map
# arc_sf <- build_sf(gm, coords_in = arcs %>%
#                      inner_join(arcs_one, "arc_") %>%
#                      inner_join(x$vertex) %>%
#                      arrange(arc_) %>% dplyr::select(x_, y_))
#
# test_that("sf build works", {
#   expect_s3_class(arc_sf, "sfc_LINESTRING")
# })

