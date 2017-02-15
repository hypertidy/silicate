#' Random IDs. 
#' 
#' See `ids` package. 
#' @param n number of bytes per ID
#' @export
#' @importFrom ids random_id
sc_rand <- function(n = 1L) ids::random_id(n, bytes = 4)
