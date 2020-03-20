## this all belongs in a core shared package
get_projection <- function(x, ...) UseMethod("get_projection")
get_projection.default <- function(x, ...) {
  crsmeta::crs_proj(x)
}
# get_projection.Spatial <- function(x, ...) {
#   x@proj4string@projargs
# }
# get_projection.BasicRaster <- function(x, ...) {
#   x@crs
# }
# get_projection.sf <- function(x, ...) {
#   get_projection(x[[attr(x, "sf_column")]]) 
# }
# get_projection.sfc <- function(x, ...) {
#   crs <- attr(x, "crs")
#   if (!is.null(crs)) {
#    if ("proj4string" %in% names(crs)) {
#      out <- unclass(crs)[["proj4string"]]
#    } else {
#      out <- "NA_character_"
#    }
#   }
#   if (is.na(out)) {
#     #warning("cannot get proj string from sf object")
#   }
#   out
# }
# get_projection.sc <- function(x, ...) {
#   x$meta$proj[1L]
# }
