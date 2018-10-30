
#' Dplyr methods for silicate objects
#'
#' dplyr methods for silicate objects, currently only `dplyr::filter` for `SC` is available.
#'
#' Apply expressions as if used on the `object` table. See `sc_object(x)` for that form.
#'
#' Currently all the vertices are still kept, so the model (and any plots) include the filtered edges as well
#' as undifferentiaed points. This is likely to change ...
#' @param .data data object of class \link{silicate-models}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr-methods
#' @export
#' @examples
#' library(dplyr)
#' sc <- SC(inlandwaters)
#' plot(filter(sc, Province == "Tasmania"))
#'
#' plot(filter(SC(minimal_mesh), a == 2))
filter.SC <- function(x, ...) {
  x[["object"]] <- dplyr::filter(x[["object"]], ...)
  tabs <- c("object", "object_link_edge", "edge")
  x[tabs] <- semi_cascade0(x[tabs], tables = tabs)
  x
}

semi_cascade0 <- function (x, ..., tables = c("o", "b", "bXv", "v")) {
  itab <- tables[1L]
  first <- dplyr::filter(x[[itab]], ...)
  x[[itab]] <- last <- first
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- semi_join_1(x[[itab]],
                                     last)
  }
  x
}

semi_join_1 <-
  function (x, y, by = NULL, copy = FALSE, ...)
  {
    comm <- base::intersect(names(x), names(y))
    if (length(comm) == 1L) {
      by <- comm
    }
    dplyr::semi_join(x, y, by = by, copy = copy, ...)
  }
