#' TRI0 model, structural triangulations
#'
#' @param x object understood by silicate (sf, sp, a silicate model, etc.)
#' @param ... current unused
#' @return TRI0 model
#' @export
TRI0 <- function(x, ...) {
  UseMethod("TRI0")
}

#' @name TRI0
#' @export
TRI0.default <- function(x, ...) {
  TRI0(PATH(x))
}

#' @name TRI0
#' @export
TRI0.PATH0 <- function(x, ...) {
 TRI0(PATH(x), ...)
}
#' @name TRI0
#' @export
TRI0.PATH <- function(x, ...) {
  vertex <- x$vertex
  if (nrow(vertex) < 3) stop("need at least 3 coordinates")
  if (anyNA(vertex$x_)) stop("missing values in x_")
  if (anyNA(vertex$y_)) stop("missing values in y_")
  path <- sc_path(x)
  if (all(path$ncoords_ < 2)) stop("TRI for PATH cannot include degenerate paths, see '.$path$ncoords_'")
  if (any(path$ncoords_ < 3)) {
    warning("filtering out paths with fewer than 3 coordinates before attempting triangulation by ear clipping")
    path <- path %>% dplyr::filter(.data$ncoords_ > 2)
  }
  ## pretty sure I'll live to regret this ...
  ## (but the right alternative is a smart DEL visibility classifier )
  ## if we get lines, just pretend they all independently POLYGON
  if (!"subobject" %in% names(path)) {
    warning("assuming that all paths are independent (i.e. all islands, no holes)")
    ##x$path$subobject <- 1
    path <- path %>% dplyr::group_by(.data$object_) %>%
      dplyr::mutate(subobject = dplyr::row_number()) %>%
      dplyr::ungroup()
  }
  x$path <- path
  tri <- triangulate_0(x)
  tri$visible <- TRUE

  tri$path_ <- NULL

  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())

  structure(list(object = obj, #object_link_triangle = oXt,
                 triangle = tri,
                 vertex = sc_vertex(x),
                 meta = meta), class = c("TRI", "sc"))
}


triangulate_00 <- function(x, ...){
  ## assume x is PATH0
  ##x <- PATH0(minimal_mesh)
  v <- sc_vertex(x)
  v$vertex_ <- 1:nrow(v)

  obj <- sc_object(x)
  count <- 0
  trilist <- list()
  for (i in seq_len(nrow(obj))) {
    ## split x$object$path_ on subobject
    topol <- obj$path_[[i]]
    lsubs <- split(topol, topol$subobject)

    ## j is sub polygons
    for (j in seq_along(lsubs)) {
      vidx <- lsubs[[j]]
      verts <- inner_join(vidx[c("vertex_", "path_")], v[c("x_", "y_", "vertex_")], "vertex_")
      ## identify holes (path_ within subobject)
      holes <- which(c(0, abs(diff(as.integer(as.factor(verts$path_))))) > 0)
      if (length(holes) < 1) holes <- 0
      count <- count + 1
      trindex <- decido::earcut(cbind(verts[["x_"]], verts[["y_"]]), holes)
      trimat <- matrix(trindex, ncol = 3L, byrow = TRUE)
      ##print(trindex)
      trilist[[count]] <- tibble::tibble(.vx0 = verts$vertex_[trimat[,1L]],
                                         .vx1 = verts$vertex_[trimat[,2L]],
                                         .vx2 = verts$vertex_[trimat[,3L]],
                                         object_ = i,
                                         path_ = lsubs[[j]]$path_[1L])


    }
  }

  ## build TRI0
  obj$path_ <- NULL
  topology_ <- dplyr::bind_rows(trilist)
  obj$topology_ <- split(topology_[c(".vx0", ".vx1",".vx2", "path_")], topology_$object_)
  meta <- x$meta[1, ]
  meta$ctime <- Sys.time()
  structure(list(object = obj, vertex = sc_vertex(x), meta = rbind(meta, x$meta)), class = c("TRI0", "sc"))
}


triangulate_0 <- function(x, ...) {
  objlist <- split(x$path, x$path$object_)
  objlist <- objlist[unique(x$path$object_)]
  polygon_count <- nrow(dplyr::distinct(x$path[c("object_", "subobject")]))
  trilist <- vector("list", polygon_count)
  itri <- 0
  for (i in seq_along(objlist)) {
    #obj <- objlist[[i]]
    #browser()
    if (length(unique(objlist[[i]]$subobject)) > 1) {
      subobjlist <- split(objlist[[i]], objlist[[i]]$subobject)
      subobjlist <- subobjlist[unique(objlist[[i]]$subobject)]
    } else {
      subobjlist <- objlist[i]
    }
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
      trilist[[itri]] <- tibble::tibble(.vx0 = verts$vertex_[trimat[,1L]],
                                        .vx1 = verts$vertex_[trimat[,2L]],
                                        .vx2 = verts$vertex_[trimat[,3L]],
                                        path_ = verts$path_[1L],
                                        object_ = objlist[[i]]$object_[1L])

    }
  }
  dplyr::bind_rows(trilist)
}



