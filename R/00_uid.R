
#' Unique labels
#'
#' Find unique labels for entities, or create them
#' if not present.
#'
#' If 'integers' default we generate sequential integers, it's assumed that all IDs are created
#' at one time, we are not adding to an existing set. Code that adds IDs should find
#' the largest existing ID and offset these by that value.
#'
#' Using 'silicate.uid.type="uuid"' is the default. Using 'silicate.uid.type="integer"' is considered experimental.
#' By default UIDs are a mix of letters, LETTERS and digits of length `getOption("silicate.uid.size")` which defaults to 6.
#'
#' See `ids` package for `random_id` used if option 'silicate.uid.type="uuid"'.
#' @param x number of unique IDs to generate
#' @param ... reserved for future use
#' @param uid_nchar number of raw characters to paste as a uuid, default is 6 (only if silicate.uid.type is "uuid", see Details)
#' @export
#' @return vector of unique id values for elements in the input
#' @examples
#' sc_uid(data.frame(1:10))
sc_uid <- function(x, ..., uid_nchar = NULL) {
  UseMethod("sc_uid")
}
#' @export
sc_uid.numeric <- function(x, ..., uid_nchar = NULL) {
  op <- getOption("silicate.uid.type")
  tst <- valid_uid_type(op)
  if (op == "uuid") {
    if (is.null(uid_nchar)) {
      uid_nchar <- getOption("silicate.uid.size")
    }
    out <- uuid_id(x[1], uid_nchar = uid_nchar)
  }
  if (op == "integer") {
    out <- seq.int(1, x[1])
  }
  out
}
#' @export
sc_uid.data.frame <- function(x, ..., uid_nchar = NULL) {
  sc_uid(nrow(x), uid_nchar = uid_nchar)
}
#' @export
sc_uid.Spatial <- function(x, ..., uid_nchar = NULL) {
  nr <- if (methods::.hasSlot(x, "data")(x)) dim(x)[1L] else length(x)
  sc_uid(nr, uid_nchar = uid_nchar)
}

uuid_id <- function(x, ..., uid_nchar = NULL) {
  #ids::random_id(x, bytes = bytes)
  unlist(lapply(split(sample(raw_chars, x * uid_nchar, replace = TRUE),
                      rep(seq.int(x), each = nchar)), paste, collapse = ""), use.names = FALSE)

}

valid_uid_type <- function(x) {
  if (!x %in% c("integer", "uuid")) {
    stop(sprintf("option 'silicate.uid.type = \"%s\"' not known (use 'integer' or 'uuid')", x))
  }
  x
}
