#' Path model in structural form
#'
#' Structural form requires only tables 'object' and 'vertex'.
#'
#' @param x an object understood by silicate
#' @param ... ignored currently
#'
#' @return PATH0 model with tables 'object' and 'vertex'
#' @export
#'
#' @examples
#' (p <- PATH0(minimal_mesh))
#'
#' p$object$topology_
PATH0 <- function(x, ...) {
  UseMethod("PATH0")
}
#' @name PATH0
#' @export
PATH0.default <- function(x, ...) {
  o <- sc_object(x)
  coord0 <- sc_coord(x)
  gmap <- gibble::gibble(x)

  ## normalize on ALL coordinate attributes, not just x_, y_ #113
  udata <- unjoin::unjoin(coord0, names(coord0), key_col = "vertex_")
  udata[["vertex_"]]$row <- seq_len(dim(udata[["vertex_"]])[1L])
  instances <- dplyr::mutate(udata[["data"]],
                             path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                             object = rep(gmap$object, gmap$nrow),
                             coord = row_number())

  if (!"subobject" %in% names(gmap)) gmap$subobject <- 1
  idx <- tibble::tibble(vertex_ = instances$vertex_,
                        object_ = rep(gmap$object, gmap$nrow),
                        ## removed as.character, path_ (and in TRI0) should not be character
                        path_ = rep(seq_len(nrow(gmap)), gmap$nrow),
                        subobject = rep(gmap$subobject, gmap$nrow))
  # if (length(unique(idx$subobject) > 1)) {
  #  #handle multis
  # }
  o$path_ <- split(idx, idx$object_)
  ## don't select just x_, y_ #113
  vertex <-     dplyr::arrange(udata[["vertex_"]], .data$vertex_)
  vertex$vertex_ <- vertex$row <- NULL
  meta <- tibble::tibble(proj = get_projection(x), ctime = Sys.time())
  structure(list(object = o, vertex = vertex, meta = meta), class = c("PATH0", "sc"))
}
#' @name PATH0
#' @export
PATH0.PATH0 <- function(x, ...) {
  x
}


#' Create a PATH0 from a data frame
#'
#' Minimal columns is x,y but can be grouped by path_ for separate paths, then subobject_ and object_ for full polygon support.
#'
#' This function exists as a special-case for non-format input for [PATH0()]. It's expected there
#' are columns x, y, and optionally object_, subobject_, and path_. These correspond to
#' names in sfheaders, multipolygon_id, polygon_id, and linestring_id. (subobject is optional if
#' not multipolygon).
#' @param x data frame with at least x, y columns
#' @param ... ignored
#' @param path_ path identifier, these should identify individual paths
#' @param object_ object identifier (like group in ggplot)
#' @param subobject_ subobject identifier (like polygon_id with multipolygons in sfheaders)
#' @param x_ optional name for x column (assumed to be x)
#' @param y_ optional name for x column (assumed to be y)
#' @name PATH0
#' @export
#' @examples
#' PATH0_from_df(data.frame(x = runif(10), y = runif(10)))
PATH0_from_df <- function(x, ..., path_ = "path_", object_ = "object_", subobject_ = "subobject_", x_ = "x", y_ = "y") {
  if (!object_ %in% names(x)) {
    x$object <- 1
  } else {
    x$object <- x[[object_]]
    x[[object_]] <- NULL
  }
  if (!path_ %in% names(x)) {
    x$path <- 1
  } else {
    x$path <- x[[path_]]
    x[[path_]] <- NULL
  }
  if (!subobject_ %in% names(x)) {
    x$subobject <- 1
  } else {
    x$subobject <- x[[subobject_]]
    x[[subobject_]] <- NULL
  }

  grp <-
    dplyr::group_by(x, .data$object, .data$path, .data$subobject)

  gmap <- ungroup(dplyr::tally(grp, name = "nrow"))
  gmap$path  <- NULL
  gmap$type <- "polygon"
  gmap$ncol <- 2L

  coord0 <- x[c(x_, y_)]
  o <- dplyr::distinct(x, .data$object)

  ## normalize on ALL coordinate attributes, not just x_, y_ #113
  udata <- unjoin::unjoin(coord0, names(coord0), key_col = "vertex_")
  udata[["vertex_"]]$row <- seq_len(dim(udata[["vertex_"]])[1L])

  instances <- dplyr::mutate(udata[["data"]],
                             path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                             object = rep(gmap$object, gmap$nrow),
                             coord = row_number())

  idx <- tibble::tibble(vertex_ = instances$vertex_,
                        object_ = rep(gmap$object, gmap$nrow),
                        ## removed as.character, path_ (and in TRI0) should not be character
                        path_ = rep(seq_len(nrow(gmap)), gmap$nrow),
                        subobject = rep(gmap$subobject, gmap$nrow))
  # if (length(unique(idx$subobject) > 1)) {
  #  #handle multis
  # }
  #browser()
  o$path_ <- split(idx, idx$object_)
  ## don't select just x_, y_ #113
  vertex <-     dplyr::arrange(udata[["vertex_"]], .data$vertex_)
  vertex$vertex_ <- vertex$row <- NULL
  names(vertex) <- c("x_", "y_")
  meta <- tibble::tibble(proj = NA_character_, ctime = Sys.time())
  structure(list(object = o, vertex = vertex, meta = meta), class = c("PATH0", "sc"))

}

