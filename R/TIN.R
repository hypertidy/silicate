# https://github.com/hypertidy/silicate/issues/118
# tin <- sf::st_as_sfc("TIN Z (((0 0 0, 0 0 1, 0 1 0, 0 0 0)), ((0 0 0, 0 1 0, 1 1 0, 0 0 0)))")

tri_to_sf <- function(x, ...) {
  x <- TRI0(x)
  d <- sc_vertex(x)[as.vector(t(as.matrix(do.call(rbind, x$object$topology_)[, c(".vx0", ".vx1", ".vx2")]))), ]
  d <- as.matrix(d)
  if (dim(d)[2L] == 2) {
    d <- cbind(d, 0)
  }
  colnames(d) <- NULL
  template <- structure(list(matrix(0.0, ncol = 3L, nrow = 4L)),
                        class = c("XYZ", "TRIANGLE", "sfg"))
  ntriangles <- dim(d)[1]/3
  out <- replicate(ntriangles, template, simplify = FALSE)
  for (i in seq_along(out)) {
    triplet <- c(1L, 2L, 3L, 1L) + (i - 1) * 3
    out[[i]][[1]] <- d[triplet, ]
  }

  rl <- unlist(lapply(x$object$topology_, function(x) dim(x)[1]))
  outlist <- lapply(split(out, rep(seq_along(rl), rl)),
                    function(x) structure(x, class = c("XYZ", "TIN", "sfg")))
  structure(outlist, n_empty = 0L,
            crs = structure(list(input = NA_character_, wkt = NA_character_), class = "crs"), class = c("sfc_TIN","sfc"),
            precision = 0, bbox = structure(c(xmin = min(d[,1L]), ymin = min(d[,2L]),  xmax = max(d[,1L]), ymax = max(d[,2L])),
                                                                             class = "bbox"),
            z_range = structure(c(zmin = min(d[,3L]), zmax = max(d[,3L])), class = "z_range"))
}


