
#' Unique labels
#' 
#' Find unique labels for entities, or create them
#' if not present. 
#' 
#' See `ids` package for `random_id` when they are created. 
#' @param x number of unique IDs to generate
#' @param ... reserved for future use
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
  if ("n" %in% names(list(...))) stop("sc_uid(): use of n argument defunct, use x = ")
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
#' @export
sc_uid.Spatial <- function(x, ...) {
  nr <- if (methods::.hasSlot(x, "data")(x)) dim(x)[1L] else length(x)
  sc_uid(nr)
}
  
  
  