#' @export
as_sc <- function(x) {
  UseMethod('as_sc')
}
#' @export
as_sc.igraph <- function(x) {
  x <- PRIMITIVE(x)
  attr(x, 'active') <- 'object'
  x
}
as_sc.sf <- function(x) {
  x <- PATH(x)
  attr(x, 'active') <- 'object'
  x
}
# sc_active <- function(x) {
#   attr(x, 'active')
# }
# `sc_active<-` <- function(x, value) {
#   attr(x, 'active') <- value
#   x
# }

#' @export
#' @importFrom tibble as_tibble
as_tibble.sc <- function(x, active = NULL, ...) {
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  switch(
    active,
    object = object_tibble(x),
    vertex = vertex_tibble(x),
    stop('Unknown active element: ', active, '. Only object or vertex supported', call. = FALSE)
  )
}


#' @importFrom tibble as_tibble
object_tibble <- function(x) {
  as_tibble(x[["object"]])
}

#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
vertex_tibble <- function(x) {
  as_tibble(x[["vertex"]])
}