
#' Dplyr methods for silicate objects
#'
#' Filter an SC model, currently only `dplyr::filter` for `SC` is available.
#'
#' Apply expressions as if used on the `object` table. See `sc_object(x)` for that form.
#'
#' Currently all the vertices are still kept, so the model (and any plots) include the filtered edges as well
#' as undifferentiated points. This is likely to change ...
#' @param .data data object of class SC
#' @param ... passed to `dplyr::filter`
#' @name dplyr-methods
#' @aliases filter
#' @export
#' @importFrom dplyr filter
#' @export filter
#' @examples
#' library(dplyr)
#' sc <- SC(inlandwaters)
#' plot(filter(sc, Province == "Tasmania"))
#'
#' plot(filter(SC(minimal_mesh), a == 2))
filter.SC <- function(.data, ...) {
  .data[["object"]] <- dplyr::filter(.data[["object"]], ...)
  tabs <- c("object", "object_link_edge", "edge")
  .data[tabs] <- semi_cascade0(.data[tabs], tables = tabs)
  .data
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


