#' TRI model, triangulations
#'
#' @inheritParams SC
#' @return TRI model
#' @export
TRI <- function(x, ...) {
  UseMethod("TRI")
}
#' @export
TRI.default <- function(x, ...) {
  TRI(PATH(x), ...)
}
#' @export
TRI.PATH <- function(x, ...) {
  #if(any(grepl("MULTIPOLYGON", x$path$type))) {
  #  message("MULTIPOLYGON ear clipping doesn't work in some cases:")
  #  message("* try `sf::st_cast(x, \"POLYGON\")` if it fails")
  #}
  tri <- triangulate.PATH(x)
  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  structure(list(object = obj, triangle = tri, 
                 vertex = sc_vertex(x)), class = c("TRI", "sc"))
}
#' @name sc_object
#' @export
sc_object.TRI <- function(x, ...) {
  x[["object"]]
}
#' @name TRI
#' @export
plot.TRI <- function(x, ...) {
  
  plot(x$vertex[c("x_", "y_")], type = "n")
  cols <- sc_colours(nrow(sc_object(x)))
  for (i in seq_len(nrow(x$object))) { 
    asub <- dplyr::filter(x$triangle, .data$object_ == x$object$object_[i]) %>% 
      dplyr::transmute(.data$.vertex0, .data$.vertex1, .data$.vertex2, fill = NA_character_) %>% 
      t() %>% 
      as.vector() 
    asub <-   tibble::tibble(vertex_ = asub)
    asub <- head(asub, -1L)
    graphics::polypath(dplyr::left_join(asub,x$vertex,  "vertex_") %>% dplyr::select(.data$x_, .data$y_), 
             col = cols[i], ...)
    
  }
}
#plot(TRI(minimal_mesh))




# dd <- minimal_mesh
# #dd <- nc
# #dd <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
# x <- SC(dd)
# plot(x)
# xt <- triangulate.SC(x, D = TRUE)
# plot(xt)
triangulate.SC <- function(x, ...) {
  v <- x$vertex
  a <- match(x$edge$.vertex0, v$vertex_)
  b <- match(x$edge$.vertex1, v$vertex_)
  p <- RTriangle::pslg(as.matrix(dplyr::select(v, .data$x_, .data$y_)), 
                       S = cbind(a, b))
  t <- RTriangle::triangulate(p, ...)
  structure(list(TRI = t$T, V = t$P), class = "TRI")
}
# plot.TRI <- function(x, ...) {
#   plot(x$V, pch = ".")
#   idx <- t(cbind(x$TRI, NA))
#   polygon(x$V[idx, ])
# }
na_split <- function(x) {
  x <- split(x[c("x_", "y_")], x$path_)[unique(x$path_)]
  if (length(x) == 1) x[[1]] else head(dplyr::bind_rows(lapply(x, function(x) rbind(dplyr::distinct(x), NA))), -1)
}

triangulate.PATH <- function(x, ...) {
  objlist <- split(x$path, x$path$object_)
  objlist <- objlist[unique(x$path$object_)]
  polygon_count <- nrow(dplyr::distinct(x$path[c("object", "subobject")]))
  trilist <- vector("list", polygon_count)
  itri <- 0
  for (i in seq_along(objlist)) {
    obj <- objlist[[i]]
    subobjlist <- split(obj, obj$subobject)
    subobjlist <- subobjlist[unique(obj$subobject)]
    for (j in seq_along(subobjlist)) {
      itri <- itri + 1
    verts <- subobjlist[[j]] %>% 
      dplyr::select(.data$object_, .data$path_) %>% 
      dplyr::inner_join(x$path[c("path_", "object_")], "path_") %>% 
      dplyr::select(.data$path_) %>% 
      dplyr::inner_join(x$path_link_vertex, "path_") %>% 
      dplyr::inner_join(x$vertex, "vertex_")
    holes <- which(c(0, abs(diff(as.integer(as.factor(verts$path_))))) > 0)
    trindex <- decido::earcut(verts[["x"]], verts[["y"]], holes)
    trimat <- matrix(trindex, ncol = 3L, byrow = TRUE)
    trilist[[itri]] <- tibble::tibble(.vertex0 = verts$vertex_[trimat[,1L]], 
                                      .vertex1 = verts$vertex_[trimat[,2L]],
                                      .vertex2 = verts$vertex_[trimat[,3L]],
                                      path_ = verts$path_[1L], 
                                      object_ = obj$object_[1L])
    
    }
  }
  dplyr::bind_rows(trilist)
}
