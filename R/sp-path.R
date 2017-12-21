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
#' @name sc_path
#' @importFrom stats setNames
#' @export
sc_path.Spatial <- function(x, ids = NULL, ...) {
  x <- gibble::gibble(x)
  if (is.null(ids)) {
    ids <- sc_uid(length(unique(x[["object"]])))
  } 
  x[["object_"]] <- ids[x[["object"]]]
  x[["path_"]] <- sc_uid(nrow(x))
  x[["ncoords_"]] <- x[["nrow"]]
  x[["nrow"]] <- NULL
  x
}
