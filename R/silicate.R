##
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

## has vertex
## has vertex + edges (edge-triangulation, drop path)
## has vertex + path (path-triangulation, no edges)
## has object (I think we must ignore unless linked by edges or path)
TRI.sc <- function(x, ...) {
  if (has_vertex(x)) {
    if (has_edge(x)) {
      ## do we build SC/PATH, or do we allow those to existing without edges or paths?
      stop("constrained triangulation not supported, use anglr::DEL or remove edges")
      ##SC(x)  ## build the object for edge-triangulation
    } else {
      if (has_path(x)) {
        ## PATH(x) ## build the object for path-triangulation
      }
      d <- x[["vertex"]]  ## pure coordinates
    }
  } else {
    stop("no vertices to triangulate")

  }
  quo_named <- rlang::quos(...)
  if (length(quo_named) < 1) {
    nms <- names(d)
    quo_named <- rlang::syms(nms)
    message(sprintf("no columns specified, using %s", paste(nms, collapse = ", ")))
  }
   out <- try(geometry::delaunayn(as.matrix(dplyr::select(d, !!!quo_named))), silent = TRUE)

  if (inherits(out, "try-error")) stop("triangulation failed") else out
}
