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
  o[["object_"]] <- sc_rand(nrow(o))
  b <- sc_path(x, ids = o[["object_"]])
  v <- sc_coord(x)
  
  key_col <- "vertex_"
  maindata <- unjoin::unjoin_(dplyr::bind_cols(v, tibble::tibble(path_ = rep(b$path_, b$ncoords_))), "path_", key_col = key_col)
  id <- sc_rand(n = nrow(maindata$data))
  v <- maindata$data %>% dplyr::mutate(vertex_ = id[maindata$data[[key_col]]])
  bXv <- maindata$main %>% dplyr::mutate(vertex_ = id[maindata$main[[key_col]]])
  #v[[key_col]] <- bXv[[key_col]] <- NULL
join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  structure(list(object = o, path = b, vertex = v, path_link_vertex = bXv), 
            class = c("PATH", "sc"), 
            join_ramp = join_ramp)
}

join_ramp <- function(x) attr(x, "join_ramp")

globalVariables(".idx0")