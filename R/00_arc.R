
#' Arc-node topology.
#'
#' Return a label and vertex count of each arc.
#'
#' Arcs are unbranched paths within the line segment graph. Nodes are the vertices where three or more arcs meet.
#'
#' As with the `PATH` and `SC` models the arc id values will only be relevant when
#' those entities are identified and labelled. Running `sc_arc` on a simple features model (for example) will
#' identify them and return a summary, but without having any record of what they refer to. Use `ARC(x)` first to
#' work with models that don't included labels.
#' @param x input object
#' @param ... arguments for methods
#' @return a data frame with only the identities of the shared boundaries (arcs)
#' @export
#' @examples
#' sc_arc(minimal_mesh)
#' ARC(minimal_mesh)[["arc"]]
#'
#' arc <- ARC(minimal_mesh)
#' plot(arc)
#' points(arc$vertex[match(sc_node(arc)$vertex_, arc$vertex$vertex_), c("x_", "y_")])
#'
#' arc <- ARC(polymesh)
#' plot(arc)
#' title("arcs and nodes")
#' points(arc$vertex[match(sc_node(arc)$vertex_, arc$vertex$vertex_), c("x_", "y_")])
sc_arc <- function(x, ...) {
  UseMethod("sc_arc")
}
#' @name sc_arc
#' @export
sc_arc.default <- function(x, ...) {
  x <- ARC(x)
  sc_arc(x, ...)
}
#' @name sc_arc
#' @export
sc_arc.ARC <- function(x, ...) {
  x[["arc_link_vertex"]] %>% dplyr::group_by(.data$arc_) %>%
    dplyr::tally() %>% dplyr::rename(ncoords_ = .data$n)
}

sc_arc_PATH <- function(x, ...) {
 path_link_vertex <- x[["path_link_vertex"]] %>%
   dplyr::inner_join(x[["path"]] %>% dplyr::select(.data$path_, .data$object_), "path_")

  sc_arc_base(path_link_vertex, sc_node(x))
}
sc_arc_SC <- function(x, ...) {
  #link_vertex <- x[["edge"]] %>% inner_join(x$object_link_edge, "edge_")
}


find_arc <- function(path, nodes) {
  path$candidate <- path$vertex_ %in% nodes

  ## rewind to put a node at the top
  idx <- which(path$candidate)
  if (idx[1L] > 1L) {
    path <- bind_rows(path[idx[1L]:nrow(path), ],
                      path[1L:(idx[1L] - 1L), ]) %>% dplyr::distinct()
    path <- bind_rows(path, path %>% dplyr::slice(1L))
    idx <- which(path$candidate)
  }

  arcs <- vector("list", length(idx) - 1)
  for (i in seq_along(arcs)) {
    arcs[[i]] <- seq(idx[i], idx[i + 1L])
  }

  tibble::tibble(object_ = path$object_[1L], arc_ = sc_uid(length(arcs))[rep(seq_along(arcs), lengths(arcs))],
                 vertex_ = path[["vertex_"]][unlist(arcs)])
}
sc_arc_base <- function(path_link_vertex, node) {
  noded_path <- dplyr::inner_join(node, path_link_vertex, "vertex_") %>%
    dplyr::distinct(.data$path_)
  ## any paths not linked to nodes are just arcs
  ## (there may be none)
  arcs0 <- path_link_vertex %>%
    dplyr::anti_join(noded_path, "path_")
  f <- factor(arcs0[["path_"]])
  arcs0[["path_"]] <- NULL
  arcs0[["arc_"]] <- sc_uid(nlevels(f))[f]
  arcs0 <- dplyr::select(arcs0, .data$arc_, .data$vertex_, .data$object_)
  path_link_vertex <- path_link_vertex %>% inner_join(noded_path, "path_")
  paths <- split(path_link_vertex, path_link_vertex[["path_"]])
  bind_rows(arcs0, lapply(paths, function(x) find_arc(x, node[["vertex_"]])))
}
