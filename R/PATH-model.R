
#' PATH model.
#'
#' A PATH model is a direct translation of a simple features-alike
#' object to normal form. This is four tables with the three kinds of entities,
#' "objects" (or "features"), "paths" (or "parts") and "vertices", and a table
#' to link the one-to-many relation between paths and vertices.
#'
#' In a data set with no parts touching their  neighbours, the only normalization of the vertices
#' will be the removal of the duplicated closing coordinate on any polygon ring, and on
#' any self-intersecting case within a single path.
#'
#' `PATH()$path` should always have columns `object_ path_ subobject ncoords_`
#' @inheritParams SC
#' @name PATH
#' @return a PATH model, with tables 'object', 'path', 'path_link_vertex' and 'vertex'
#' @seealso `sc_path`, `sc_coord`
#' @export
PATH <- function(x, ...) UseMethod("PATH")

PATH.PATH <- function(x, ...) x
#' @name PATH
#' @export
PATH.SC <- function(x, ...) {
  stop("PATH not yet implemented for SC")
}
#' @name PATH
#' @export
PATH.TRI <- function(x, ...) {
  stop("PATH not yet implemented for TRI")
}

#' @name PATH
#' @export
#' @importFrom dplyr bind_cols mutate
#' @importFrom tibble tibble
#' @importFrom unjoin unjoin
PATH.default  <- function(x, ...) {
  ## get the main stuff
  o <- sc_object(x)
  o[["object_"]] <- sc_uid(nrow(o))
  b <- sc_path(x, ids = o[["object_"]])

  v <- sc_coord(x)
  V_names <- names(v)
  v <- dplyr::mutate(v, path_ = rep(b$path_, b$ncoords_))
  key_col <- "vertex_"
  maindata <- unjoin::unjoin(v, V_names, key_col = key_col)
  dd <- maindata[["data"]]
  id <- sc_uid(dd)
  v <- dplyr::mutate(maindata[[key_col]], vertex_ = id[maindata[[key_col]][[key_col]]])
  bXv <- dplyr::mutate(maindata[["data"]], vertex_ = id[dd[[key_col]]])
  #v[[key_col]] <- bXv[[key_col]] <- NULL
  join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  meta <- tibble(proj = get_projection(x), ctime = format(Sys.time(), tz = "UTC"))
  structure(list(object = o, path = b,path_link_vertex = bXv, vertex = v, meta = meta),
            class = c("PATH", "sc"),
            join_ramp = join_ramp)
}

join_ramp <- function(x) attr(x, "join_ramp")

globalVariables(".idx0")
