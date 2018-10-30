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
#' print(BINARY(minimal_mesh))
#' print(PATH(minimal_mesh))
#'
#' print(BINARY(inlandwaters))
print.sc <- function(x, ...) {
  mt <- model_type(x)
  topology <- topology_type(mt)
   cat(sprintf("class       : %s\n", class(x)[1]))
   cat(sprintf("type        : %s\n", mt))
   cat(sprintf("vertices    : %s\n", nrow(x$vertex)))
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
         BINARY = "Primitive",
         SC0 = "Structural",
         TRI = "Primitive",
         DEL = "Primitive",
         SC = "Primitive",
         ARC = "Sequential",
         PATH = "Sequential",
         "[Unknown]")
}

topology_type <- function(x) {
  switch(tail(class(x), 2)[1],
         BINARY = sprintf("%i-space", ncol(x$object$topology_[[1]])),
         TRI = "2-space",
         DEL = "2-space",
         SC = "1-space",
         SC0 = "implicit",
         ARC = "arc",
         PATH = "path",
         "[unknown]")
}

n_primitives <- function(x) {
  switch(tail(class(x), 2)[1],
         BINARY = sum(unlist(lapply(x$object$topology_, nrow))),
         TRI = nrow(x$triangle),
         DEL = nrow(x$triangle),
         SC = nrow(x$edge),
         SC0 = nrow(x$segment),
         "[unknown]")
}

n_paths <- function(x) {
  switch(tail(class(x), 2)[1],
  ARC = length(unique(x$object_link_arc$arc_)),
  PATH = nrow(x$path),
  SC0 = nrow(x$geometry),
  "[unknown")
}
n_coordinates <- function(x) {
  switch(tail(class(x), 2)[1],
         ARC = nrow(x$arc_link_vertex),  ## FIXME: arc may become "arc" table, remove object_link_arc
         PATH = sum(x$path$ncoords_),
         SC0 = nrow(x$coord),
         "[unknown")
}
