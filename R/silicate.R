# nocov start
include_nn <- function(x, y, name) {
  if (!is.null(y)) x[[name]] <- tibble::as_tibble(y)
  x
}
has_thing <- function(x, thing) {
  thing %in% names(x) && tibble::is_tibble(x[[thing]])
}
has_vertex <- function(x) {
  has_thing(x, "vertex")
}

has_edge <- function(x) {
 has_thing(x, "edge")
}
has_path <- function(x) {
  has_thing(x, "path")
}
has_object <- function(x) {
  has_thing(x, "object")
}

silicate <- function(vertex = NULL, edge = NULL, path = NULL, object = NULL, model = NULL) {
  x <- model %||% list()
  x <- include_nn(x, vertex, "vertex")
  x <- include_nn(x, edge, "edge")
  x <- include_nn(x, path, "path")
  x <- include_nn(x, object, "object")

  if (length(x) == 0L) warning("empty silicate shell")
  structure(x, class = "sc")
}

# nocov end
