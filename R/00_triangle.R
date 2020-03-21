## this all stuff belongs in silicate (rgl)
## pondering sc_quad (or just sc_primitive, because quads can be triangles)



sc_triangle <- function(x, ...) {
  UseMethod("sc_triangle")
}

sc_triangle.default <- function(x, ...) {
  sc_triangle(silicate::TRI0(x), ...)
}

sc_triangle.TRI0 <- function(x, ...) {
  obj <- sc_object(x)
  #obj[["object_"]] <- seq_len(nrow(obj))
  tidyr::unnest(obj["topology_"], cols = c("topology_"), .id = "object_")
}

sc_triangle.TRI <- function(x, ...) {
  x[["triangle"]]
}

sc_triangle.mesh3d <- function(x, ...) {
  tri <- x[["it"]]
  if (is.null(tri)) stop("no triangles in this model")  ## could convert quads, though

  tibble::tibble(.vx0 = tri[1L, ],
                 .vx1 = tri[2L, ],
                 .vx2 = tri[3L, ])  ## visible = TRUE, object_ = 1 ??
}
