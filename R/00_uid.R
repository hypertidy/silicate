
#' Unique labels
#' 
#' Find unique labels for entities, or create them
#' if not present. 
#' 
#' See `ids` package for `random_id` when they are created. 
#' @param x number of unique IDs to generate
#' @param ... reserved for future use
#' @export
sc_uid <- function(x, ..., bytes = 5L) {
  UseMethod("sc_uid")
}
#' @export
#' @importFrom ids random_id
sc_uid.default <- function(x, ..., bytes = 5L) {
  if ("n" %in% names(list(...))) stop("sc_uid(): use of n argument defunct, use x = ")
  #ids::random_id(x, bytes = bytes)
  seq.int(1, x)
}
#' @export
sc_uid.numeric <- function(x, ..., bytes = 5L) {
  #ids::random_id(x, bytes = bytes)
  seq.int(1, x)
}
#' @export
sc_uid.data.frame <- function(x, ..., bytes = 5L) {
  return(seq.int(1, nrow(x)))
  rn <- row.names(x)
  nr <- dim(x)[1L]
  if (identical(rn, as.character(seq_len(nr)))) {
    rn <- sc_uid(nr, bytes = bytes)
  }
  rn
}
#' @export
sc_uid.Spatial <- function(x, ..., bytes = 5L) {
  nr <- if (methods::.hasSlot(x, "data")(x)) dim(x)[1L] else length(x)
  return(seq.int(1, nr))
  sc_uid(nr, bytes = bytes)
}


  
# sc_uid <- function(x, ..., bytes = 5L) {
#    UseMethod("sc_uid")
# }
# sc_uid.numeric <- function(x, ...) {
#   seq.int(1, n)
# }
# sc_uid.default <- function(x, ...) {
#   seq.int(1, nrow(x))
# }
#   