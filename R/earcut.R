## internal function
earcut_PATH <- function(x, ...) {
  holes <-  NULL
  path <- silicate::sc_path(x)
  path_link_vertex <- x$path_link_vertex
  vertex <- x$vertex
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  ## here we need a new "object", analogous to polygon
  ## can be temporary
  path$hole <- duplicated(path$object_)
  #path$object_ <- as.integer(factor(path$object_))
  tri <- unlist(lapply(unname(split(path, path$object_)), function(apath) {
    x <- dplyr::inner_join(apath, path_link_vertex, "path_") %>%
      dplyr::inner_join(vertex, "vertex_") %>%
      dplyr::select(.data$x_, .data$y_, .data$vertex_, .data$path_)
    x <- x[!duplicated(x["vertex_"]), ]
    holes <- which(abs(diff(as.integer(factor(x$path_)))) > 0)
    if (length(holes) < 1) holes <- 0
    idx <- decido::earcut(cbind(x[["x_"]], x[["y_"]]), holes, ...)
    out <- match(x$vertex_[idx], vertex$vertex_)
    out
  }))
  tri
}

