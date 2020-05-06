#' Path model in structural form
#'
#' Structural form requires only tables 'object' and 'vertex'.
#'
#' @param x an object understood by silicate
#' @param ... ignored currently
#'
#' @return PATH0 model with tables 'object' and 'vertex'
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
  coord0 <- sc_coord(x)
  gmap <- gibble::gibble(x)

  ## normalize on ALL coordinate attributes, not just x_, y_ #113
  udata <- unjoin::unjoin(coord0, names(coord0), key_col = "vertex_")
  udata[["vertex_"]]$row <- seq_len(dim(udata[["vertex_"]])[1L])
  instances <- dplyr::mutate(udata[["data"]],
                             path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                             object = rep(gmap$object, gmap$nrow),
                             coord = row_number())

  if (!"subobject" %in% names(gmap)) gmap$subobject <- 1
  idx <- tibble::tibble(vertex_ = instances$vertex_,
                        object_ = rep(gmap$object, gmap$nrow),
                        ## removed as.character, path_ (and in TRI0) should not be character
                        path_ = rep(seq_len(nrow(gmap)), gmap$nrow),
                        subobject = rep(gmap$subobject, gmap$nrow))
  # if (length(unique(idx$subobject) > 1)) {
  #  #handle multis
  # }
  o$path_ <- split(idx, idx$object_)
  ## don't select just x_, y_ #113
  vertex <-     dplyr::arrange(udata[["vertex_"]], .data$vertex_)
  vertex$vertex_ <- vertex$row <- NULL
  meta <- tibble::tibble(proj = get_projection(x), ctime = Sys.time())
  structure(list(object = o, vertex = vertex, meta = meta), class = c("PATH0", "sc"))
}
#' @name PATH0
#' @export
PATH0.PATH0 <- function(x, ...) {
  x
}
