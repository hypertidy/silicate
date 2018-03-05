#' TRI model, triangulations
#'
#' @inheritParams SC
#' @param add logical create  new plot (default), or add to existing
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
  vertex <- x$vertex
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  tri <- triangulate_PATH(x)
  tri$visible <- TRUE
 
  tri$triangle_ <- sc_uid(nrow(tri))
  oXt <- dplyr::distinct(tri[c("object_", "triangle_")])
  tri$object_ <- NULL
  tri$path_ <- NULL

  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())
  
  structure(list(object = obj, object_link_triangle = oXt, triangle = tri, 
                 vertex = sc_vertex(x), meta = meta), class = c("TRI", "sc"))
}
#' @name sc_object
#' @export
sc_object.TRI <- function(x, ...) {
  x[["object"]]
}
#' @name TRI
#' @export
plot.TRI <- function(x, ..., add = FALSE) {
  
  if (!add) plot(x$vertex[c("x_", "y_")], type = "n")
  cols <- sc_colours(nrow(sc_object(x)))
  for (i in seq_len(nrow(x$object))) { 
    triangle <- dplyr::inner_join(x$triangle, x$object_link_triangle)
    asub <- dplyr::filter(triangle, .data$object_ == x$object$object_[i]) %>% 
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




na_split <- function(x) {
  x <- split(x[c("x_", "y_")], x$path_)[unique(x$path_)]
  if (length(x) == 1) x[[1]] else head(dplyr::bind_rows(lapply(x, function(x) rbind(dplyr::distinct(x), NA))), -1)
}

triangulate_PATH <- function(x, ...) {
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
    if (length(holes) < 1) holes <- 0
    trindex <- decido::earcut(cbind(verts[["x_"]], verts[["y_"]]), holes)
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
