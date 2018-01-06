
#' Unique labels
#' 
#' Find unique labels for entities, or create them
#' if not present. 
#' 
#' See `ids` package for `random_id` when they are created. 
#' @param n number of bytes per ID
#' @export
sc_uid <- function(x, ...) {
  UseMethod("sc_uid")
}
#' @export
#' @importFrom ids random_id
sc_uid.default <- function(x, ...) {
  if (is.numeric(x)) {
    warning("old behaviour where input is integer to be removed")
  }
  ids::random_id(x, bytes = 4L)
}
#' @export
sc_uid.numeric <- function(x, ...) {
  ids::random_id(x, bytes = 4L)
}
#' @export
sc_uid.data.frame <- function(x, ...) {
  rn <- row.names(x)
  nr <- dim(x)[1L]
  if (identical(rn, as.character(seq_len(nr)))) {
    rn <- sc_uid(nr)
  }
  rn
}
sc_uid.Spatial <- function(x, ...) {
  nr <- if (spbabel:::has_data(x)) dim(x)[1L] else length(x)
  sc_uid(nr)
}
  
  
  