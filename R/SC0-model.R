path_paste <- function(x, paster = function(...) paste(..., sep = "-")) {
  ## we are looking for  any of these three
  do.call(paster, x[intersect(names(x), c("object", "subobject", "path"))])
}


c.SC0 <- function(...) {
  all_list <- list(...)
  nrs <- integer(length(all_list))
  for (i in seq_along(all_list)) {
    all_list[[i]]$object$id <- i
    nrs[i] <- nrow(all_list[[i]]$vertex)
  }
  ## object here means which element of all_list
#print(nrs)
  if (length(nrs) > 1) {
    incr <- 0
  for (i in seq_along(all_list)) {

    for (j in seq_along(all_list[[i]]$object$topology_)) {
    all_list[[i]]$object[["topology_"]][[j]]$.vx0 <- all_list[[i]]$object[["topology_"]][[j]]$.vx0 + incr
    all_list[[i]]$object[["topology_"]][[j]]$.vx1 <- all_list[[i]]$object[["topology_"]][[j]]$.vx1 + incr
    }
    incr <- incr + nrs[i]
    #all_list[[i]]$object["topology_"] <- topo1
  }
  }

  segs <- purrr::map_df(all_list, ~tidyr::unnest(.x$object["topology_"], cols = c(.data$topology_)), .id = "object")
  ## id here means the same (but it didn't exist before)
  #objs <- purrr::map_df(all_list, ~.x$object["id"])

  vert <- purrr::map_df(all_list, ~.x$vertex[c("x_", "y_")])
  vdata <- unjoin::unjoin(vert, .data$x_, .data$y_)
 # if (nrow(vdata$data) == nrow(vdata$.idx0)) {
    ## we done, just bind the tables after re-indexing

#  } else {
 #   browser()
    uverts <- vdata$.idx0
    segs$.vx0 <- match(vdata$data$.idx0[segs$.vx0], vdata$.idx0$.idx0)
    segs$.vx0 <- match(vdata$data$.idx0[segs$.vx1], vdata$.idx0$.idx0)
    vert <- uverts[c("x_", "y_")]
#  }

  topology <- split(segs[c(".vx0", ".vx1")], segs$object)[unique(segs$object)]

  # incr <- 0
  # for (i in seq_along(topology)) {
  #   topology[[i]]$.vx0 <- topology[[i]]$.vx0 + incr
  #   topology[[i]]$.vx1 <- topology[[i]]$.vx1 + incr
  #   incr <- incr + nrs[i]
  #   print(i)
  # }
  out <- structure(list(object = tibble::tibble(topology_ = topology), vertex = vert,
                        meta = tibble::tibble(proj = all_list[[1]]$meta$proj[1], ctime = Sys.time())), class = c("SC0", "sc"))
  out
}


normalize_to_vertices <- function(x, ..., .keep_all = FALSE) {
  coord0 <- sc_coord(x)
  if (all(c("x_", "y_","z_", "t_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$z_, .data$t_, key_col = "vertex_")
    return(out)
  }
  if (all(c("x_", "y_","z_", "m_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$z_, .data$m_, key_col = "vertex_")
    return(out)
  }
  if (all(c("x_", "y_","z_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$z_,  key_col = "vertex_")
    return(out)
  }
  if (all(c("x_", "y_","t_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$t_,  key_col = "vertex_")
    return(out)
  }

  if (all(c("x_", "y_","m_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$m_,  key_col = "vertex_")
    return(out)
  }
  if (all(c("x_", "y_","m_") %in% names(coord0))) {
    out <- unjoin::unjoin(coord0, .data$x_, .data$y_, .data$m_,  key_col = "vertex_")
    return(out)
  }

  ## need more cases, but by now we assume x_, y_
  unjoin::unjoin(coord0, .data$x_, .data$y_,   key_col = "vertex_")
}


#' Pure edge model, structural form
#'
#' `SC0` is the simplest and most general of all silicate models. Composed of
#' an `object` and `vertex` table linked by nested vertex-index pairs.
#'
#'
#' @param x an object understood by silicate
#' @param ... reserved for methods
#'
#' @return SC0 model with tables 'object' and 'vertex'
#' @export
#'
#' @examples
#' SC0(minimal_mesh)
#' SC0(minimal_mesh)
SC0 <- function(x, ...) {
  UseMethod("SC0")
}
#' @name SC0
#' @importFrom dplyr mutate slice group_by ungroup select
#' @importFrom gibble gibble
#' @export
SC0.default <- function(x, ...) {
  #coord0 <- sc_coord(x)
  #udata <- unjoin::unjoin(coord0, .data$x_, .data$y_, key_col = "vertex_")
  udata <- normalize_to_vertices(x)
  udata[["vertex_"]]$row <- seq_len(nrow(udata[["vertex_"]]))
  gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
  instances <- udata$data %>% dplyr::mutate(path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                                            object = rep(gmap$object, gmap$nrow),
                                            coord = row_number())

  object <- sc_object(x)
  if (length(unique(instances$path)) == nrow(instances)) {
    ## we are only points
    #   stop(sprintf("no segments/edges found in object of class %s", class(x)))
    instances[".vx0"] <- instances["vertex_"]
    object$topology_ <- split(instances[c(".vx0")], instances$object)

  } else {
    ## cx0 and cx1 are the segment vertices, they map the coordinate instances, not the vertices
    segs <- instances %>% dplyr::select(.data$path, .data$coord, .data$object)  %>%
      dplyr::mutate(.cx0 = .data$coord,   ## specify in segment terms
                    .cx1 = .data$coord + 1L) %>%
      dplyr::group_by(.data$path) %>% dplyr::slice(-dplyr::n()) %>% dplyr::ungroup() %>%
      dplyr::transmute(.data$.cx0, .data$.cx1, path_ = .data$path, .data$object)

    segs[[".vx0"]] <- instances$vertex_[match(segs$.cx0, instances$coord)]
    segs[[".vx1"]] <- instances$vertex_[match(segs$.cx1, instances$coord)]
    ## but udata$.idx0 has the vertices, with .idx0 as the mapping
    object$topology_ <- split(segs[c(".vx0", ".vx1", "path_")], segs$object)

  }
  meta <- tibble(proj = get_projection(x), ctime = Sys.time())
vertex <- udata$vertex_ %>%
  dplyr::arrange(.data$vertex_)

bad <- !names(vertex) %in% c("x_", "y_", "z_", "m_", "t_")
  if (any(bad)) vertex <- vertex[!bad]
  structure(list(object = object, vertex = vertex,
                 meta = meta),
            class = c("SC0", "sc"))
}
#' @export
SC0.SC0 <- function(x, ...) {
  ## this should de-duplicated vertices so we can combine
  ## multiple models - see https://github.com/mdsumner/spacebucket/issues/4
  x
}
#' @export
SC0.TRI <- function(x, ...) {
  ## this should be SC0(TRI0(x))
  triangle <- x$triangle
  tritri <- matrix(match(unlist(  triangle[c(".vx0", ".vx1", ".vx2")]), x$vertex$vertex_), ncol = 3)
  triangle[[".vx0"]] <- tritri[,1L, drop = TRUE]
  triangle[[".vx1"]] <- tritri[,2L, drop = TRUE]
  triangle[[".vx2"]] <- tritri[,3L, drop = TRUE]
  
  triangle_list <- split(triangle, triangle$object_)[unique(triangle$object_)]
  top <- vector("list", length(triangle_list))
  for (i in seq_along(triangle_list)) {
    top[[i]] <- purrr::map_df(purrr::transpose(triangle_list[[i]]),
                              ~tibble::as_tibble(matrix(tri_to_seg(unlist(.x[c(".vx0", ".vx1", ".vx2")])), ncol = 2, byrow = TRUE, dimnames = list(NULL, c(".vx0", ".vx1")))))
  }
  object <- sc_object(x)
  object[["object_"]] <- NULL
  object[["topology_"]] <- top
  vertex <- sc_vertex(x)
  meta <- x$meta[1, ]
  meta$ctime <- Sys.time()
  structure(list(object = object, vertex = vertex, meta = rbind(meta, x$meta)), class = c("SC0", "sc"))

}
#' @export
SC0.SC <- function(x, ...) {
  object <- sc_object(x)
  oXe <- x$object_link_edge %>%
    dplyr::inner_join(sc_edge(x), "edge_")
  .vx  <- cbind(.vx0 = match(oXe[[".vx0"]], x$vertex[["vertex_"]]),
                .vx1 = match(oXe[[".vx1"]], x$vertex[["vertex_"]]))
  ## swap order if not native instance
  swap <- !oXe[["native_"]]
  ## doing my head in atm ... could be better
  if (length(which(swap)) > 0) .vx  <- cbind(.vx0 = .vx[cbind(1:nrow(.vx), swap + 1)],
                                             .vx1 = .vx[cbind(1:nrow(.vx), (!swap) + 1)])
  object[["topology_"]] <- split(tibble::as_tibble(.vx), as.integer(factor(oXe$object_,unique(oXe$object_))))
  structure(list(object = object, vertex = sc_vertex(x) %>% dplyr::mutate(vertex_ = NULL)), class = c("SC0", "sc"))
}
sc_vertex.SC0 <- function(x, ...) {
  x[["vertex"]]
}


