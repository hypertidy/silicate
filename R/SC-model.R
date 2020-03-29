globalVariables("n")


#' The universal model
#'
#' The universal model `SC` is coordinates and binary relations between
#' pairs of coordinates. This is purely an edge (or segment) model, with all
#' higher level structures recorded as groupings of edges.
#' @param x input model
#' @param ... arguments passed to methods
#' @export
#' @return SC model with tables 'object', 'object_link_edge', 'edge', and 'vertex'
#' @examples
#' ## we can produce a high quality triangulation from a low quality one
#' ## see how the TRI edges are maintained (we can't yet filter out holes from DEL)
#' tri <- TRI(minimal_mesh)
#' plot(tri)
#' plot(SC(tri))
SC <- function(x, ...) {
  UseMethod("SC")
}

#' #' @export
#' #' @name SC
#' SC.SC0 <- function(x, ...) {
#'   v <- sc_vertex(x)
#'   o <- sc_object(x)
#'   index <- do.call(rbind, o$topology_)
#'   o$topology_ <- NULL
#'   structure(list(object = O,
#'                  object_link_edge = oXe,
#'                  edge = edge,
#'                  vertex = V,
#'                  meta = meta),
#'             ## a special join_ramp, needs edge to split on vertex
#'             join_ramp = c("object", "object_link_edge", "edge", "vertex"),
#'             class = c("SC", "sc"))
#' }

#' @export
#' @name SC
SC.default <- function(x, ...) {
  B <- SC0(x, ...)
  O <- sc_object(B)
  O$topology_ <- NULL
  if (!"object_" %in% names(O)) O[["object_"]] <- sc_uid(O)
  O1 <- O["object_"]
  O1[["edge_"]] <- B$object[["topology_"]]
  meta <- tibble::tibble(proj = get_projection(x), ctime = format(Sys.time(), tz = "UTC"))
  for (i in seq_along(O1$edge_)) O1$edge_[[i]]$object_ <- O1$object_[i]
  edge <- do.call(rbind, O1$edge_)
  tst <- c(".vx0", ".vx1") %in% names(edge)
  if (!all(tst)) {
    if (sum(tst) == 1) stop("model has only 0-space vertices (is it point-topology? Try '?SC0'. )")
    stop("unable to produce edge form of this data")
  }
  V <- sc_vertex(B)
  if (!"vertex_" %in% names(V)) V[["vertex_"]] <- sc_uid(V)
  ## these are now the edges, but we need to classify which changed direction
  v_0 <- pmin(edge$.vx0, edge$.vx1)
  v_1 <- pmax(edge$.vx0, edge$.vx1)
  edge$native_ <- v_0 == edge$.vx0  ## if TRUE the orientation is how it came in

  ## we now have ordered edges
  edge[[".vx0"]] <- V[["vertex_"]][v_0]
  edge[[".vx1"]] <- V[["vertex_"]][v_1]

  edge[["u_edge"]] <- dplyr::group_indices(edge, .data$.vx0, .data$.vx1)
  edge[["edge_"]] <- sc_uid(length(unique(edge$u_edge)))[edge$u_edge]
  oXe <- edge[c("object_", "edge_", "native_")]
  edge$native_ <- edge$object_ <- NULL
  edge <- edge[!duplicated(edge$u_edge), ]
  edge$object_ <- edge$u_edge <- NULL
  structure(list(object = O,
                 object_link_edge = oXe,
                 edge = edge,
                 vertex = V,
                 meta = meta),
            ## a special join_ramp, needs edge to split on vertex
            join_ramp = c("object", "object_link_edge", "edge", "vertex"),
            class = c("SC", "sc"))
}


## triangle classification
#' @name SC
#' @export
SC.TRI <- function(x, ...) {
  segment <- purrr::map_df(purrr::transpose(x$triangle[c(".vx0", ".vx1", ".vx2")]),
                           ~to_tibble(tri_to_seg(unlist(.x))), .id = "triangle_")
 edges <- as.integer(factor(apply(cbind(segment$.vx0, segment$.vx1), 1,
                                   function(x) paste(sort(x), collapse = "-"))))
  segment$edge_ <- sc_uid(length(unique(edges)))[edges]
  segment$object_ <- x$triangle$object_[as.numeric(segment$triangle_)]
  object_link_edge <- dplyr::distinct(segment, .data$object_, .data$edge_, .data$object_)
  object_link_edge[["native_"]] <- TRUE ## always native
  segment <- segment[c(".vx0", ".vx1", "edge_")] %>% inner_join(object_link_edge, "edge_") %>%
    dplyr::transmute(.vx0 = .data$.vx0, .vx1 = .data$.vx1, edge_ = .data$edge_)

  structure(list(object = x$object, object_link_edge = object_link_edge,
                 edge = segment, vertex = x$vertex,
                 meta = rbind(dplyr::mutate(x$meta, ctime = Sys.time()), x$meta)), class = c("SC", "sc"))
}




## need to identify segments that were input and are
## shared by two triangles, set to invisible
tri_to_seg <- function(x) {
  x[c(1, 2, 2, 3, 3, 1)]
}

to_tibble <- function(x) {
  mat <- matrix(x, ncol = 2, byrow = TRUE)
  colnames(mat) <- c(".vx0", ".vx1")
  tibble::as_tibble(mat)
}

##https://github.com/hypertidy/silicate/issues/46
ring_cycles <- function(aa) {
  ii <- 1
  set0 <- ii
  visited <- logical(nrow(aa))
  while(!all(visited)) {
    i0 <- ii
    repeat {
      ii <- which(aa[,1] == aa[ii, 2])
      if (length(ii) < 1 | ii[1] == i0) {
        set0 <- c(set0, NA_integer_)
        break;
      }
      set0 <- c(set0, ii)
    }
    visited <- seq(nrow(aa)) %in% stats::na.omit(set0)
    ii <- which(!visited)[1L]
    if (!is.na(ii)) set0 <- c(set0, ii)
  }
  l <- split(set0, c(0, cumsum(abs(diff(is.na(set0))))))
  bind_rows(lapply(l[!unlist(lapply(l, function(x) all(is.na(x))))],
                   function(x) tibble(row = x)), .id = "cycle")
}


