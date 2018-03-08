globalVariables("n")


#' The universal model
#' 
#' The universal model `SC` is coordinates and binary relations between
#' pairs of coordinates. This is purely an edge (or segment) model, with all 
#' higher level structures recorded as groupings of edges. 
#' @param x input model
#' @param ... arguments passed to methods
#' @export
#' @examples 
#' ## we can produce a high quality triangulation from a low quality one
#' ## see how the TRI edges are maintained (we can't yet filter out holes from DEL)
#' tri <- TRI(minimal_mesh)
#' plot(tri)
#' 
#' plot(DEL(SC(TRI(minimal_mesh)), max_area = 0.001))
#' 
#' ## Nice small triangles in a conforming Delaunay mesh. 
#' plot(SC(DEL(simpleworld[121, ], D = TRUE, max_area = .1)))
SC <- function(x, ...) {
  UseMethod("SC")
}
#' @export
SC.default <- function(x, ...) {
  P <- PATH(x, ...)
  O <- sc_object(P)
  S <- sc_segment(P)
  E <- dplyr::select(S, .data$.vertex0, .data$.vertex1, .data$edge_) %>% 
    dplyr::distinct(.data$edge_, .keep_all = TRUE)
  ExO <- S %>% 
    dplyr::select(.data$path_, .data$edge_) %>% 
    dplyr::inner_join(dplyr::select(P[["path"]], .data$path_, .data$object_), "path_") %>% 
    dplyr::distinct(.data$edge_, .data$object_) 
  #join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  meta <- tibble(proj = get_projection(x), ctime = format(Sys.time(), tz = "UTC"))
  
  structure(list(object = O,
                 object_link_edge = ExO,
                 edge = E, 
                 vertex = sc_vertex(P),
                 meta = meta), 
            ## a special join_ramp, needs edge to split on vertex
            join_ramp = c("object", "object_link_edge", "edge", "vertex"),
            class = c("SC", "sc"))
}




sc_segment.SC <- function(x, ...) {
  ## expand all instances of edges
  segments <- x$object_link_edge %>% 
    inner_join(x$edge)
  
    ## and badge them as segments
  segments$segment_ <- sc_uid(nrow(segments))
  segments
}
sc_path_link_vertex.SC <- function(x,  ...) {
  segments <- sc_segment(x)
  ## iterate all objects and find all paths
  objects <- split(segments, segments$object_)[unique(segments$object_)]
  out <- vector("list", length(objects))
  for (i in seq_along(objects)) {
    obj <- objects[[i]]
    rc <- ring_cycles(as.matrix(obj[c(".vertex0", ".vertex1")]))
    obj$path_ <- sc_uid(length(unique(rc$cycle)))[factor(rc$cycle)]
    out[[i]] <- obj %>% dplyr::group_by(.data$path_) %>% 
      dplyr::rename(vertex_ = .data$.vertex0) %>%
      dplyr::select(.data$vertex_, .data$path_) %>% 
      ## n() here relies on globalVariables declaration
      dplyr::slice(c(1:n(), 1)) %>% 
      dplyr::ungroup()
  }
  object <- bind_rows(out)
  ## and split out object grouping from path grouping
  object
}



## need to identify segments that were input and are
## shared by two triangles, set to invisible
tri_to_seg <- function(x) {
  x[c(1, 2, 2, 3, 3, 1)]
}

to_tibble <- function(x) {
  setNames(tibble::as_tibble(matrix(x, ncol = 2, byrow = TRUE)), c(".vertex0", ".vertex1"))
}
## triangle classification
#' @name SC
#' @export 
SC.TRI <- function(x, ...) {
  segment <- purrr::map_df(purrr::transpose(x$triangle[c(".vertex0", ".vertex1", ".vertex2")]), 
                           ~to_tibble(tri_to_seg(unlist(.x))), .id = "triangle_")
  segment$triangle_ <- x$triangle$triangle_[as.integer(segment$triangle_)]
  #segment$segment_ <- silicate::sc_uid(nrow(segment))
  edges <- as.integer(factor(apply(cbind(segment$.vertex0, segment$.vertex1), 1, 
                                   function(x) paste(sort(x), collapse = "-"))))
  segment$edge_ <- sc_uid(length(unique(edges)))[edges]
  object_link_edge <- dplyr::inner_join(x$object_link_triangle, 
                                        segment[c("edge_", "triangle_")]) %>% 
    dplyr::distinct(object_, edge_)
  edge <- dplyr::distinct(segment %>% dplyr::select(.vertex0, .vertex1, edge_), 
                          edge_, .keep_all = TRUE)
  #object_link_edge <- tibble(object_ = x$object_link_triangle$object_[match(edge$triangle_, x$object_link_triangle$triangle_)],  
  #                           segment_ = segment$segment_)
  structure(list(object = x$object, object_link_edge = object_link_edge, 
                 edge = edge, vertex = x$vertex, 
                 meta = rbind(dplyr::mutate(x$meta, ctime = Sys.time()), x$meta)), class = c("SC", "sc"))
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


## experimental
filter.SC <- function(x, ...) {
  x[["object"]] <- dplyr::filter(x[["object"]], ...)
  tabs <- c("object", "object_link_edge", "edge")
  x[tabs] <- semi_cascade0(x[tabs], tables = tabs)
  x
}

semi_cascade0 <- function (x, ..., tables = c("o", "b", "bXv", "v")) {
  itab <- tables[1L]
  first <- dplyr::filter(x[[itab]], ...)
  x[[itab]] <- last <- first
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- semi_join_1(x[[itab]], 
                                     last)
  }
  x
}

semi_join_1 <- 
  function (x, y, by = NULL, copy = FALSE, ...) 
  {
    comm <- base::intersect(names(x), names(y))
    if (length(comm) == 1L) {
      by <- comm
    }
    dplyr::semi_join(x, y, by = by, copy = copy, ...)
  }