#' Generate a BRANCH model. 
#' 
#' A BRANCH model is a direct translation of the a simple features-alike
#' model to normal form. 
#' @param x input model
#' @param ... arguments passed to methods
#' @name BRANCH
#' @seealso `sc_branch`, `sc_coord`
#' @export
#' @examples
#' library(sf)
#' sf_dataset <- st_sf(geometry = st_sfc(sfzoo[[2]]), a = 1)
#' BRANCH(sf_dataset)
BRANCH <- function(x, ...) UseMethod("BRANCH")

## a function sf should have
## to drop the spatial stuff
.st_set_geometry <- function(x, value = NULL) {
  st_geometry(x) <- value
  x
}

#' @name BRANCH
#' @export
BRANCH.default  <- function(x, ...) {
  o <- tibble::as_tibble(.st_set_geometry(x))
  o[["object_"]] <- sc_rand(nrow(o))
  b <- sc_branch(x, ids = o[["object_"]])
  v <- sc_coord(x)
  paste_ <- function(...) paste(..., sep = "_")
  v_factor <- factor(do.call(paste_, v))
  id <- sc_rand(n = nlevels(v_factor))
  bXv <- tibble::tibble(branch_ = rep(b$branch_, b$ncoords_))
  v[["vertex_"]] <- bXv[["vertex_"]] <- id[v_factor]
  v <- dplyr::distinct_(v, "vertex_", .keep_all = TRUE)
  join_ramp <-  tabnames <- c("object", "branch",  "branch_link_vertex", "vertex")
  structure(list(object = o, branch = b, vertex = v, branch_link_vertex = bXv), 
            class = c("BRANCH", "sc"), 
            join_ramp = join_ramp)
}

join_ramp <- function(x) attr(x, "join_ramp")