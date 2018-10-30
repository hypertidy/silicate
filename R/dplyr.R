

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
