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
  out <- dplyr::bind_rows(lapply(.sp_get_geometry(x), sc_path), .id = "object")
  if (is.null(ids)) ids <- sc_uid(length(unique(out[["object"]])))
  out[["object"]] <- ids[as.integer(out[["object"]])]
  out
}
#' @name sc_path
#' @export
sc_path.Polygons <- function(x, ...) {
  out <- tibble::as_tibble(do.call(rbind, lapply(x@Polygons, function(xa) cbind(ncoords_ = nrow(xa@coords), ncol = 2L))))
  out[["type"]] <- "Polygon"
  out[["path"]] <- sc_uid(nrow(out))
  out
}
#' @name sc_path
#' @export
sc_path.Lines<- function(x, ...) {
  out <- tibble::as_tibble(do.call(rbind, lapply(x@Lines, function(xa) cbind(ncoords_ = nrow(xa@coords), ncol = 2L))))
  out[["type"]] <- "Line"
  out[["path"]] <- sc_uid(nrow(out))
  out
}
#' @name sc_path
#' @export
sc_path.default <- function(x, ...) {
  out <- tibble::as_tibble(cbind(ncoords_ = nrow(x), ncol = 2L))
  out[["type"]] <- "Point"
  out[["path"]] <- sc_uid(nrow(out))
  
  out
  
}