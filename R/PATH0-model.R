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


  udata <- unjoin::unjoin(coord0, .data$x_, .data$y_, key_col = "vertex_")
  udata[["vertex_"]]$row <- seq_len(nrow(udata[["vertex_"]]))
  #gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
  instances <- udata$data %>% dplyr::mutate(path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                                            object = rep(gmap$object, gmap$nrow),
                                            coord = row_number())





  if (!"subobject" %in% names(gmap)) gmap$subobject <- 1
  idx <- tibble::tibble(vertex_ = instances$vertex_,
                        object_ = rep(gmap$object, gmap$nrow),
                        path_ = as.character(rep(seq_len(nrow(gmap)), gmap$nrow)),
                        subobject = rep(gmap$subobject, gmap$nrow))
  if (length(unique(idx$subobject) > 1)) {
   #handle multis
  }
  o$path_ <- split(idx, idx$object_)
  vertex <- udata$vertex_ %>%
    dplyr::arrange(.data$vertex_) %>% dplyr::select(.data$x_, .data$y_)
  meta <- tibble::tibble(proj = get_projection(x), ctime = Sys.time())
  structure(list(object = o, vertex = vertex, meta = meta), class = c("PATH0", "sc"))
}
#' @name PATH0
#' @export
PATH0.PATH0 <- function(x, ...) {
  x
}
