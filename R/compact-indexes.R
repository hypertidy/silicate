sc_compact <- function(x, ...) {
  UseMethod("sc_compact")
}
match_int <- function(x, y, ...) {
  match(x, y)
}
sc_compact.SC <- function(x, ...) {
  oXe <- x[["object_link_edge"]]
  oXe$object_ <- match_int(oXe$object_, x$object$object_)
  oXe$edge_ <- match_int(oXe$edge_, x$edge$edge_)

  
  x$object$object_ <- NULL
  x[["object_link_edge"]] <- oXe
  
  edge <- x[["edge"]]
  edge$.vertex0 <- match_int(edge$.vertex0, x$vertex$vertex_)
  edge$.vertex1 <- match_int(edge$.vertex1, x$vertex$vertex_)
  edge$edge_ <- NULL
  
  x[["edge"]] <- edge
  x$vertex$vertex_ <- NULL
  structure(x, class = c("compact_SC", "sc"))
}

sc_expand <- function(x, ...) {
  UseMethod("sc_expand")
}
sc_expand.compact_SC <- function(x, ...)  {
  x$object$object_ <- sc_uid(x$object)
  oXe <- x[["object_link_edge"]]
  edge <- x[["edge"]]
  vertex <- x[["vertex"]]
  edge$edge_ <- sc_uid(edge)
  vertex$vertex_ <- sc_uid(vertex)
  
  oXe$object_ <- x$object$object_[oXe$object_]
  oXe$edge_ <- edge$edge_[oXe$edge_]
  edge$.vertex0 <- vertex$vertex_[edge$.vertex0]
  edge$.vertex1 <- vertex$vertex_[edge$.vertex1]
  x[["object_link_edge"]] <- oXe
  x[["edge"]] <- edge
  x[["vertex"]] <- vertex
  structure(x, class = c("SC", "sc"))
}
