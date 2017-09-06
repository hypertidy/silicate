#' Generate a PATH model. 
#' 
#' A PATH model is a direct translation of a simple features-alike
#' object to normal form. This is four tables with the three kinds of entities, 
#' "objects" (or "features"), "paths" (or "parts") and "vertices", and a table 
#' to link the one-to-many relation between paths and vertices. 
#' 
#' In a data set with no parts touching their  neighbours, the only normalization of the vertices
#' will be the removal of the duplicated closing coordinate on any polygon ring, and on
#' any self-intersecting case within a single path. 
#' @param x input model
#' @param ... arguments passed to methods
#' @name PATH
#' @seealso `sc_path`, `sc_coord`
#' @export
PATH <- function(x, ...) UseMethod("PATH")


#' @name PATH
#' @export
#' @importFrom dplyr bind_cols mutate
#' @importFrom tibble tibble
PATH.default  <- function(x, ...) {
  ## get the main stuff
  o <- sc_object(x)
  o[["object"]] <- sc_uid(nrow(o))
  b <- sc_path(x, ids = o[["object"]])
  
  v <- sc_coord(x)
  V_names <- names(v)
  v <- dplyr::mutate(v, path_ = rep(b$path_, b$ncoords_))
  key_col <- "vertex_"
  maindata <- unjoin::unjoin_(v, V_names, key_col = key_col)
  dd <- maindata[["data"]]
  id <- sc_uid(n = nrow(dd))
  v <- dplyr::mutate(maindata[[key_col]], vertex_ = id[maindata[[key_col]][[key_col]]])
  bXv <- dplyr::mutate(maindata[["data"]], vertex_ = id[dd[[key_col]]])
  #v[[key_col]] <- bXv[[key_col]] <- NULL
  join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  structure(list(object = o, path = b, vertex = v, path_link_vertex = bXv), 
            class = c("PATH", "sc"), 
            join_ramp = join_ramp)
}

join_ramp <- function(x) attr(x, "join_ramp")

globalVariables(".idx0")
