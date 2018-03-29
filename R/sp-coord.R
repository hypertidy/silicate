#' @importFrom methods slotNames
.sp_get_geometry <- function(x) {
  slt <- slotNames(x)
  if ("polygons" %in% slt) {
    return(slot(x, "polygons"))
  }
  if ("lines" %in% slt) {
    return(slot(x, "lines"))
  }
  out <- slot(x, "coords")
  if (!is.recursive(out)) out <- list(out)
  out
}
#' @name sc_coord
#' @importFrom stats setNames
#' @export
sc_coord.Spatial <- function(x, ...) {
  stats::setNames(tibble::as_tibble(do.call(rbind, lapply(.sp_get_geometry(x), sc_coord))), c("x_", "y_"))
}
#' @name sc_coord
#' @export
sc_coord.Polygons <- function(x, ...){
  do.call(rbind, lapply(x@Polygons, function(xa) xa@coords))
}
#' @name sc_coord
#' @export
sc_coord.Lines<- function(x, ...){
  do.call(rbind, lapply(x@Lines, function(xa) xa@coords))
}
