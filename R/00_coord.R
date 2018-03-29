#' Coordinate decomposition
#'
#' Collect all instances of all coordinates in one table. This complementary to the `sc_path` of
#' an object, since the number of coordinates per path gives a structural mapping into this set.
#'
#' @param x input model
#' @param ... arguments passed to methods
#'
#' @name sc_coord
#' @export
#' @seealso `sc_path` for the central part of the model, `sc_object` for
#' the features, and `PATH` for the full model.
sc_coord <- function(x, ...) UseMethod("sc_coord")

#' @name sc_coord
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join transmute_ select_
sc_coord.PATH <- function(x, ...) {
  dplyr::inner_join(dplyr::transmute(x[["path_link_vertex"]], vertex_ = .data$vertex_) ,
                    x[["vertex"]], "vertex_") %>%
    dplyr::select(-.data$vertex_)
}

#' @name sc_coord
#' @export
sc_coord.default <- function(x, ...){
   x[["coord"]]  %||% stop("no coord present", call. = FALSE)
}
