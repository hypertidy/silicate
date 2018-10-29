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
   if (mt == "Primitive") {
   cat(sprintf("primitives  : %s (%ss)\n", n_primitives(x), topology_type(x)))
   }
   if (mt == "Sequential") {
   cat(sprintf("paths       : %s (%s)\n", n_paths(x), topology_type(x)))
   cat(sprintf("coordinates : %s\n", n_coordinates(x)))

   }
   cat(sprintf("crs         : %s\n", get_projection(x)))

   invisible(NULL)
}



model_type <- function(x) {

  switch(tail(class(x), 2)[1],
         BINARY = "Primitive",
         TRI = "Primitive",
         DEL = "Primitive",
         SC = "Primitive",
         ARC = "Sequential",
         PATH = "Sequential",
         "[Unknown]")
}

topology_type <- function(x) {
  switch(tail(class(x), 2)[1],
         BINARY = "segment",
         TRI = "triangle",
         DEL = "triangle",
         SC = "segment",
         ARC = "arc",
         PATH = "path",
         "[unknown]")
}

n_primitives <- function(x) {
  switch(tail(class(x), 2)[1],
         BINARY = sum(unlist(lapply(x$object$edge_, nrow))),
         TRI = nrow(x$triangle),
         DEL = nrow(x$triangle),
         SC = nrow(x$edge),

         "[unknown]")
}

n_paths <- function(x) {
  switch(tail(class(x), 2)[1],
  ARC = length(unique(x$object_link_arc$arc_)),
  PATH = nrow(x$path),
  "[unknown")
}
n_coordinates <- function(x) {
  switch(tail(class(x), 2)[1],
         ARC = nrow(x$arc_link_vertex),  ## FIXME: arc may become "arc" table, remove object_link_arc
         PATH = sum(x$path$ncoords_),
         "[unknown")
}
