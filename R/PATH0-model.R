#' Path model in structural form
#'
#' Structural form requires only tables 'object' and 'vertex'.
#'
#' @param x an object understood by silicate
#' @param ... ignored currently
#'
#' @return PATH0 model
#' @export
#'
#' @examples
#' (p <- PATH0(minimal_mesh))
#'
#' p$object$topology_
PATH0 <- function(x, ...) {
  UseMethod("PATH0")
}
#' @name PATH0
#' @export
PATH0.default <- function(x, ...) {
  o <- sc_object(x)
  v <- sc_coord(x)
  g <- gibble::gibble(x)
  if (!"subobject" %in% names(g)) g$subobject <- 1
  idx <- tibble::tibble(vertex_ = seq_len(sum(g$nrow)),
                        object_ = rep(g$object, g$nrow),
                        path_ = rep(seq_len(nrow(g)), g$nrow),
                        subobject_ = rep(g$subobject, g$nrow))
  if (length(unique(idx$subobject_) > 1)) {
   #handle multis
  }
  o$path_ <- split(idx, idx$object_)
  meta <- tibble::tibble(proj = get_projection(x), ctime = Sys.time())
  structure(list(object = o, vertex = v, meta = meta), class = c("PATH0", "sc"))
}
#' @name PATH0
#' @export
PATH0.PATH0 <- function(x, ...) {
  x
}
