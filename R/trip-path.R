
#' @export
sc_path.trip <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x)
  if (is.null(ids)) {
    ids <- sc_uid(length(unique(x[["object_"]])))
  } 
  x[["object_"]] <- ids[x[["object_"]]]
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}

#' @export
sc_coord.trip <- function(x, ...) {
  tor <- slot(x, "TOR.columns")
  dt <- x[[tor[1L]]]
  data <- slot(x, "data")
  data <- data[setdiff(names(data), tor)]
  x <- tibble::as_tibble(slot(x, "coords"))
  x <- stats::setNames(x, c("x_", "y_"))
  x[["t_"]] <- dt
  dplyr::bind_cols(x, data)

}
#' @export
sc_object.trip <- function(x, ...) {
  tor <- slot(x, "TOR.columns")
  tibble::tibble(trip = unique(x[[tor[2L]]]))
}