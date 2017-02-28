#' @importFrom utils head
#' @importFrom tibble as_tibble
path_to_segment <- function(x, id = NULL) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  x <- stats::setNames(tibble::as_tibble(utils::head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)), 
                       c(".vertex0", ".vertex1"))
  if (!is.null(id)) x[["path_"]] <- id
  x
}

#' @importFrom tibble as_tibble
p2seg <- function(x) tibble::as_tibble(path_to_segment(x$vertex_))
