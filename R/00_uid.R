
#' Unique labels
#'
#' Find unique labels for entities, or create them
#' if not present.
#'
#' By default we generate sequential integers, it's assumed that all IDs are created
#' at one time, we are not adding to an existing set. Code that adds IDs should find
#' the largest existing ID and offset these by that value.
#'
#' Using 'silicate.uid.type="uuid"' is considered experimental.
#'
#' See `ids` package for `random_id` used if option 'silicate.uid.type="uuid"'.
#' @param x number of unique IDs to generate
#' @param ... reserved for future use
#' @param bytes number of bytes passed to 'ids' package (only if silicate.uid.type is "uuid", see Details)
#' @export
sc_uid <- function(x, ..., bytes = 5L) {
  UseMethod("sc_uid")
}
#' @export
#' @importFrom ids random_id
sc_uid.numeric <- function(x, ..., bytes = 5L) {
  op <- getOption("silicate.uid.type")
  if (op == "uuid") {
    uuid_id(x[1], bytes = bytes)
  } else {
    seq.int(1, x[1])
  }
}
#' @export
sc_uid.data.frame <- function(x, ..., bytes = 5L) {
  op <- getOption("silicate.uid.type")
  if (op == "uuid") {
    row.names(x)
  } else {
    sc_uid(nrow(x))
  }
}
#' @export
sc_uid.Spatial <- function(x, ..., bytes = 5L) {
  nr <- if (methods::.hasSlot(x, "data")(x)) dim(x)[1L] else length(x)
  sc_uid(nr, bytes = bytes)
}


uuid_id <- function(x, ..., bytes = 5L) {
  ids::random_id(x, bytes = bytes)
}

valid_uid_type <- function(x) {
  if (!x %in% c("integer", "uuid")) {
    warning(sprintf("option 'silicate.uid.type = \"%s\"' not known (use 'integer' or 'uuid')", x))
  }
  x
}
