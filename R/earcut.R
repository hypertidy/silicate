#' Ear cut triangulation
#' 
#' Ear cutting triangulation for polygons. 
#' 
#' This function builds on `earcut` the package `decido`. 
#' @name earcut
#' @export
#' @importFrom decido earcut
#' @export earcut
#' @export 
#' @param x PATH object to triangulate
#' @param holes ignored by the PATH method
#' @param ... 
#' @examples 
#' earcut.PATH(PATH(minimal_mesh))
earcut.PATH <- function(x, holes = NULL, ...) {
  path <- silicate::sc_path(x)
  path_link_vertex <- x$path_link_vertex
  vertex <- x$vertex
  ## here we need a new "object", analogous to polygon
  ## can be temporary
  path$hole <- duplicated(path$object)
  #path$object_ <- as.integer(factor(path$object_))
  tri <- unlist(lapply(unname(split(path, path$object_)), function(apath) {
    x <- dplyr::inner_join(apath, path_link_vertex, "path_") %>%
      dplyr::inner_join(vertex, "vertex_") %>% 
      dplyr::select(.data$x_, .data$y_, .data$vertex_, .data$path_)
    x <- x[!duplicated(x["vertex_"]), ]
    holes <- which(abs(diff(as.integer(factor(x$path_)))) > 0)
    if (length(holes) < 1) holes <- 0
    idx <- earcut(x[["x_"]], x[["y_"]], holes, ...)
    out <- match(x$vertex_[idx], vertex$vertex_)
    out
  }))
  tri
}

#' @name earcut
#' @export
earcut.sf <- function(x, ...) {
  earcut.PATH(PATH(x), ...)
}
#' @name earcut
#' @export
earcut.sfc <- function(x, ...) {
  earcut.PATH(PATH(x), ...)
}
