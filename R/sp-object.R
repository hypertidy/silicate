#' @export
#' @importFrom methods .hasSlot
sc_object.Spatial <- function(x, ...) {
  
  if (!.hasSlot(x, "data")) {
    out <- setNames(list(seq_along(x)), class(x))
  } else {
    out <- slot(x, "data")
  }
  tibble::as_tibble(out)
}