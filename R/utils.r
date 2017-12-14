#' @importFrom utils head
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
path_to_segment <- function(x, id = NULL) {
  ## this is a trick of array logic to generate paired indexes from a sequence
  x <- stats::setNames(tibble::as_tibble(utils::head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)), 
                       c(".vertex0", ".vertex1"))
  if (!is.null(id)) x[["path"]] <- id
  x
}

#' @importFrom tibble as_tibble
p2seg <- function(x) tibble::as_tibble(path_to_segment(x$vertex_))


faster_as_tibble <- function(x) {
  if (is.matrix(x)) x <- split(as.vector(x), rep(seq_len(ncol(x)), each = nrow(x)))
  dm <- try(dim(x), silent = TRUE)
  ## kludge a bit because x may be a list or a 0-column dataframe
  nr <- if (inherits(dm, "try-error")) length(x[[1]]) else dm
  structure(x, row.names = nr, class = c("tbl_df", "tbl", "data.frame"))
}
