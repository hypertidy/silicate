## four polygon rings, two islands
## (copy the first island translated to give )
a1 <- cbind(c(-1, -1, 1, 1, -1),
      c(-1, 1, 1, -1, -1))
h1 <- cbind(c(-0.5, -0.5, 0, -0.5),
            c(0, -0.5, -0.5, 0))
h2 <- h1 + 0.6
a3 <- a1 + 6
library(sf)
## this is the "target"
#mpol <- st_multipolygon(list(list(a1, h1, h2),
#                             list(a1 + 3, h1 + 3, h2 + 3),
#                             list(a3)))

#
mlin <- st_multilinestring(list(a1, h1, h2,
                                a1 + 3, h1 + 3, h2 + 3,
                                a3))
g <- st_polygonize(mlin)

## see how h1 and h2 are present as both holes and islands
# GEOMETRYCOLLECTION (
#   POLYGON ((-1 -1, -1 1, 1 1, 1 -1, -1 -1),
#            (-0.5 0, -0.5 -0.5, 0 -0.5, -0.5 0),
#            (0.1 0.6, 0.1 0.1, 0.6 0.1, 0.1 0.6)),
#   POLYGON ((-0.5 0, 0 -0.5, -0.5 -0.5, -0.5 0)),
#   POLYGON ((0.1 0.6, 0.6 0.1, 0.1 0.1, 0.1 0.6)),
## and see how the pattern is clear, we simply get the holes as fill
#   POLYGON ((2 2, 2 4, 4 4, 4 2, 2 2),
#            (2.5 3, 2.5 2.5, 3 2.5, 2.5 3),
#            (3.1 3.6, 3.1 3.1, 3.6 3.1, 3.1 3.6)),
#   POLYGON ((2.5 3, 3 2.5, 2.5 2.5, 2.5 3)),
#   POLYGON ((3.1 3.6, 3.6 3.1, 3.1 3.1, 3.1 3.6)),
#   POLYGON ((5 5, 5 7, 7 7, 7 5, 5 5)))

library(dplyr)
gpolygon <- lapply(g, function(x) st_polygon(x))
gatomic <- unlist(gpolygon, recursive = FALSE)

## ALL THIS WORK is pointless, we have inferred the policy
## of the GC returned from GEOS, it should just be explicit
## so we have some clarity from that lib

## make a map of where we are going
gmap <- gibble::gibble(gpolygon)   %>%
  group_by(object) %>%
  mutate(hole = cumsum(subobject) > 1) %>% ungroup() %>%
  mutate(id = "")
## loop over paths, digest in assumed clockwise order
for (i in seq_len(nrow(gmap))) {
  mat <- gatomic[[i]]
  ## pretty sure GEOS will reliably return holes reversed compared to islands
  if (gmap$hole[i]) mat <- mat[rev(seq_len(nrow(mat))), ]
  gmap$id[[i]] <- digest::digest(mat)
}
## gmap where not-hole, but duplicate is to be removed
bad <- (!gmap$hole) & duplicated(gmap$id)
if (any(bad)) gpolygon <- gpolygon[-which(bad[!gmap$hole])]



