#' @export
#' @param n see `sc_uid`
#' @name sc-deprecated
sc_rand <- function(n = 1L) {
  .Deprecated("sc_uid") 
  sc_uid(n = n)
} 
#' Random IDs. 
#' 
#' See `ids` package. 
#' @param n number of bytes per ID
#' @export
#' @importFrom ids random_id
sc_uid <- function(n = 1L) {
  ids::random_id(n, bytes = 4)
}
