
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
#' @param nchar number of raw characters to paste as a uuid, default is 10 (only if silicate.uid.type is "uuid", see Details)
#' @export
sc_uid <- function(x, ..., nchar = 10L) {
  UseMethod("sc_uid")
}
#' @export
sc_uid.numeric <- function(x, ..., nchar = 10L) {
  op <- getOption("silicate.uid.type")
  if (op == "uuid") {
    uuid_id(x[1], nchar = nchar)
  } else {
    seq.int(1, x[1])
  }
}
#' @export
sc_uid.data.frame <- function(x, ..., nchar = 10L) {
  op <- getOption("silicate.uid.type")
  if (op == "uuid") {
    row.names(x)
  } else {
    sc_uid(nrow(x))
  }
}
#' @export
sc_uid.Spatial <- function(x, ..., nchar = 10L) {
  nr <- if (methods::.hasSlot(x, "data")(x)) dim(x)[1L] else length(x)
  sc_uid(nr, nchar = nchar)
}

uuid_id <- function(x, ..., nchar = 10) {
  #ids::random_id(x, bytes = bytes)
  unlist(lapply(split(sample(raw_chars, x * nchar, replace = TRUE),
                      rep(seq.int(x), each = nchar)), paste, collapse = ""))

}

valid_uid_type <- function(x) {
  if (!x %in% c("integer", "uuid")) {
    warning(sprintf("option 'silicate.uid.type = \"%s\"' not known (use 'integer' or 'uuid')", x))
  }
  x
}
