
library(sf)
example(st_read)
minimal_mesh
library(igraph)
gr <-graph(t(PRIMITIVE(nc[4, ])$segment[, 1:2]))
plot(gr)


el <- as_edgelist(gr, names=FALSE)

loops.e <- which(el[,1] == el[,2])
nonloops.e <- which(el[,1] != el[,2])
loops.v <- el[,1] [loops.e]
loop.labels <- edge.labels[loops.e]
loop.labx <- if (is.null(elab.x)) {
  rep(NA, length(loops.e))
} else {
  elab.x[loops.e]
}
loop.laby <- if (is.null(elab.y)) {
  rep(NA, length(loops.e))
} else {
  elab.y[loops.e]
}
edge.labels <- edge.labels[nonloops.e]
elab.x <- if (is.null(elab.x)) NULL else elab.x[nonloops.e]
elab.y <- if (is.null(elab.y)) NULL else elab.y[nonloops.e]
el <- el[nonloops.e,,drop=FALSE]
