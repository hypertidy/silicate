#' Path model in structural form
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PATH0 <- function(x, ...) {
  UseMethod("PATH0")
}
#' @name PATH0
#' @export
PATH0.default <- function(x, ...) {
  o <- sc_object(x)
  v <- sc_coord(x)
  g <- gibble::gibble(x)
  idx <- tibble::tibble(vertex_ = seq_len(sum(g$nrow)),
                        object = rep(g$object, g$nrow),
                        path = rep(seq_len(nrow(g)), g$nrow),
                        subobject = rep(g$subobject, g$nrow))
  if (length(unique(idx$subobject) > 1)) {
   #handle multis
  }
  o$path <- split(idx, idx$object)
  structure(list(object = o, vertex = v), class = c("PATH0", "sc"))
}
