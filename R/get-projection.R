## this all belongs in a core shared package
get_projection <- function(x, ...) UseMethod("get_projection")
get_projection.default <- function(x, ...) {
  NA_character_
}
get_projection.Spatial <- function(x, ...) {
  x@proj4string@projargs
}
get_projection.BasicRaster <- function(x, ...) {
  x@crs
}
get_projection.sf <- function(x, ...) {
  attr(x[[attr(x, "sf_column")]], "crs")[["proj4string"]]
}
get_projection.sfc <- function(x, ...) {
  attr(x, "crs")[["proj4string"]]
}
get_projection.sc <- function(x, ...) {
  x$meta$proj[1L]
}
