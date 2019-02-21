# nocov start
sc_compact <- function(x, ...) {
  .Defunct("compact_indexes")
  #UseMethod("sc_compact")
}
## Compact form, structural indexing
#
## @inheritParams SC
#
## @return a special "compact_*" form, of e.g.  SC, or PATH
## @noRd
## @examples
## compact_indexes(SC(minimal_mesh))
compact_indexes <- function(x, ...) {
  .Defunct()
  return(NULL)
#  UseMethod("compact_indexes")
}

match_int <- function(x, y, ...) {
  match(x, y)
}
compact_indexes.SC <- function(x, ...) {

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
## @name compact_indexes
compact_indexes.PATH <- function(x, ...) {
  path <- x[["path"]]
  path$object_ <- match_int(path$object_, x$object$object_)
  x$object$object_ <- NULL

  pXv <- x[["path_link_vertex"]]
  pXv$path_ <- match_int(pXv$path_, path$path_)
  path$path_ <- NULL
  x[["path"]] <- path
  pXv$vertex_ <- match_int(pXv$vertex_, x$vertex$vertex_)
  x[["path_link_vertex"]] <- pXv
  x$vertex$vertex_ <- NULL
  structure(x, class = c("compact_PATH", "sc"))
}
sc_expand <- function(x, ...) {
  .Defunct("expand_indexes")
  UseMethod("sc_expand")
}


## Expand indexes from structural compaction
#
## @inheritParams SC
#
## @return non-compact form, i.e. SC or PATH
## @noRd
## @examples
## small <- compact_indexes(PATH(minimal_mesh))
## large <- expand_indexes(small)
expand_indexes <- function(x, ...) {
  .Defunct()
  return(NULL)
  UseMethod("expand_indexes")
}
## @name expand_indexes
expand_indexes.compact_SC <- function(x, ...)  {
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
## @name expand_indexes
expand_indexes.compact_PATH <- function(x, ...) {
  x$object$object_ <- sc_uid(x$object)
  path <- x[["path"]]
  pXv <- x[["path_link_vertex"]]
  vertex <- x[["vertex"]]
  path$path_ <- sc_uid(path)
  path$object_ <- x$object$object_[path$object_]

  pXv$path_ <- path$path_[pXv$path_]
  vertex$vertex_ <- sc_uid(vertex)
  pXv$vertex_ <- vertex$vertex_[pXv$vertex_]
  x[["path"]] <- path
  x[["path_link_vertex"]] <- pXv
  x[["vertex"]] <- vertex
  structure(x, class = c("PATH", "sc"))
}
# nocov end
