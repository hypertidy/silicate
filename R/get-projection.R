## this all belongs in a core shared package
get_projection <- function(x, ...) UseMethod("get_projection")
get_projection.default <- function(x, ...) {
  ## same as anglr:::get_proj 2020-03-24
  x_na <- NA_character_
  proj <- crsmeta::crs_proj(x)
  if (is.na(proj) || is.null(proj) || nchar(proj) < 1) {
    proj <- crsmeta::crs_input(x)
  }
  if (is.na(proj) || is.null(proj) || nchar(proj) < 1) {
    return(x_na)
  }
  proj
}
