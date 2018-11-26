# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a



#' @importFrom utils tail
path_to_segment <- function(x) tibble::as_tibble(list(.vx0 = utils::head(x, -1L),
                                                       .vx1 = utils::tail(x, -1)))

