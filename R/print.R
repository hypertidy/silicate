#' Methods for silicate
#'
#' Print a silicate model.
#'
#' Simple summary of type and content of a silicate model.
#' @param x object inheriting from 'sc' class
#' @param ... ignore currently
#' @export
#' @examples
#' print(TRI(minimal_mesh))
#' print(SC(minimal_mesh))
#' print(PATH(minimal_mesh))
#' print(SC(TRI(minimal_mesh)))
#' print(ARC(minimal_mesh))
#' print(SC0(minimal_mesh))
print.sc <- function(x, ...) {
  mt <- model_type(x)
  topology <- topology_type(mt)
   cat(sprintf("class       : %s\n", class(x)[1]))
   cat(sprintf("type        : %s\n", mt))
   cat(sprintf("vertices    : %s (%i-space)\n", nrow(x$vertex), n_geometry(x)))
   if (mt == "Primitive" || mt == "Structural") {

   cat(sprintf("primitives  : %s (%s)\n", n_primitives(x), topology_type(x)))
   }
   if (mt == "Sequential" || mt == "Structural") {
   cat(sprintf("paths       : %s (%s)\n", n_paths(x), topology_type(x)))
   cat(sprintf("coordinates : %s\n", n_coordinates(x)))

   }
   cat(sprintf("crs         : %s\n", get_projection(x)))

   invisible(NULL)
}



model_type <- function(x) {

  switch(tail(class(x), 2)[1],

         SC0 = "Structural",
         PATH0 = "Sequential",
         TRI = "Primitive",
         TRI0 = "Primitive",
         DEL = "Primitive",
         DEL0 = "Primitive",
         SC = "Primitive",
         ARC = "Sequential",
         PATH = "Sequential",
         "[Unknown]")
}

topology_type <- function(x) {
  switch(tail(class(x), 2)[1],
         TRI = "2-space",
         DEL = "2-space",
         TRI0 = "2-space",
         DEL0 = "2-space",
         SC = "1-space",
         SC0 = sprintf("%i-space", ncol(x$object$topology_[[1]]) - 1),
         PATH0 = "1-space",
         ARC = "1-space",
         PATH = "1-space",
         "[unknown]")
}

n_primitives <- function(x) {
  switch(tail(class(x), 2)[1],
         SC0 = sum(unlist(lapply(x$object$topology_, nrow))),
         TRI = nrow(x$triangle),
         DEL = nrow(x$triangle),
         TRI0 = sum(unlist(lapply(x$object$topology_, nrow))),
         DEL0 = sum(unlist(lapply(x$object$topology_, nrow))),
         SC = nrow(x$edge),
         "[unknown]")
}
n_geometry <- function(x) {
   length(intersect(c("x_", "y_", "z_", "t_", "m_"), names(x$vertex)))
}
n_paths <- function(x) {
  switch(tail(class(x), 2)[1],
  ARC = length(unique(x$object_link_arc$arc_)),
  PATH = nrow(x$path),
  SC0 = nrow(x$geometry),
  PATH0 = sum(unlist(lapply(x$object$path_, function(a) length(unique(a$path_))))),
  "[unknown")
}
n_coordinates <- function(x) {
  switch(tail(class(x), 2)[1],
         ARC = nrow(x$arc_link_vertex),  ## FIXME: arc may become "arc" table, remove object_link_arc
         PATH = sum(x$path$ncoords_),
         PATH0 = nrow(x$vertex),
         SC0 = nrow(x$coord),
         "[unknown")
}
