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
#' @examples
#' library(sf)
#' sf_dataset <- st_sf(geometry = st_sfc(sfzoo[[2]]), a = 1)
#' PATH(sf_dataset)
PATH <- function(x, ...) UseMethod("PATH")

## a function sf should have
## to drop the spatial stuff
#' @importFrom sf st_geometry<-
.st_set_geometry <- function(x, value = NULL) {
  #st_geometry(x) <- value
  x[[attr(x, "sf_column")]] <- NULL
  as.data.frame(x)
}

#' @name PATH
#' @export
PATH.default  <- function(x, ...) {
  o <- tibble::as_tibble(.st_set_geometry(x))
  o[["object_"]] <- sc_rand(nrow(o))
  b <- sc_path(x, ids = o[["object_"]])
  v <- sc_coord(x)
  paste_ <- function(...) paste(..., sep = "_")
  v_factor <- factor(do.call(paste_, v))
  id <- sc_rand(n = nlevels(v_factor))
#<<<<<<< HEAD:R/BRANCH-model.r
#  bXv <- tibble::tibble(branch_ = rep(b$branch_, b$ncoords_))
#  bXv[["order_"]] <- unlist(lapply(split(bXv[["branch_"]], bXv[["branch_"]]), seq_along))
#=======
  bXv <- tibble::tibble(path_ = rep(b$path_, b$ncoords_))
  v[["vertex_"]] <- bXv[["vertex_"]] <- id[v_factor]
  v <- dplyr::distinct_(v, "vertex_", .keep_all = TRUE)
  join_ramp <-  tabnames <- c("object", "path",  "path_link_vertex", "vertex")
  structure(list(object = o, path = b, vertex = v, path_link_vertex = bXv), 
            class = c("PATH", "sc"), 
            join_ramp = join_ramp)
}

join_ramp <- function(x) attr(x, "join_ramp")