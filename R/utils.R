# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a



#' @importFrom utils tail
path_to_segment <- function(x) tibble::as_tibble(list(.vx0 = utils::head(x, -1L),
                                                       .vx1 = utils::tail(x, -1)))


## duplicated in anglr, originally from quadmesh::trianglate_quads
.quad2tri <- function (quad_index, clockwise = FALSE)
{
  if (clockwise) {
    matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L,
                                                           3L, 4L), ]), 3L)
  }
  else {
    matrix(rbind(quad_index[c(1L, 4L, 2L), ], quad_index[c(4L,
                                                           3L, 2L), ]), 3L)
  }
}
