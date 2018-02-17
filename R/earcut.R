#' @name earcut
#' @export
#' @importFrom rearcut earcut
earcut.PATH <- function(x, ...) {
  path <- silicate::sc_path(x)
  path_link_vertex <- x$path_link_vertex
  vertex <- x$vertex
  ## here we need a new "object", analogous to polygon
  ## can be temporary
  path$hole <- duplicated(path$object)
  #path$object_ <- as.integer(factor(path$object_))
  tri <- unlist(lapply(unname(split(path, path$object_)), function(apath) {
    x <- dplyr::inner_join(apath, path_link_vertex, "path_") %>%
      dplyr::inner_join(vertex, "vertex_") %>% dplyr::select(x_, y_, vertex_, path_)
    x <- x[!duplicated(x["vertex_"]), ]
    holes <- which(abs(diff(as.integer(factor(x$path_)))) > 0)
    
    idx <- earcut(x[c("x_", "y_")], holes)
    out <- match(x$vertex_[idx], vertex$vertex_)
    out
  }))
  tri
}
