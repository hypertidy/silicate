#' #' @export
#' as_sc <- function(x) {
#'   UseMethod('as_sc')
#' }
#' #' @export
#' as_sc.igraph <- function(x) {
#'   x <- PRIMITIVE(x)
#'   attr(x, 'active') <- 'object'
#'   x
#' }
#' as_sc.sf <- function(x) {
#'   x <- PATH(x)
#'   attr(x, 'active') <- 'object'
#'   x
#' }
#' # sc_active <- function(x) {
#' #   attr(x, 'active')
#' # }
#' # `sc_active<-` <- function(x, value) {
#' #   attr(x, 'active') <- value
#' #   x
#' # }
#' 
#' #' @export
#' #' @importFrom tibble as_tibble
#' #' @export as_tibble
#' as_tibble.sc <- function(x, active = NULL, ...) {
#'   if (is.null(active)) {
#'     active <- attr(x, 'active')
#'   }
#'   switch(
#'     active,
#'     object = object_tibble(x),
#'     vertex = vertex_tibble(x),
#'     stop('Unknown active element: ', active, '. Only object or vertex supported', call. = FALSE)
#'   )
#' }
#' 
#' 
#' #' @importFrom tibble as_tibble
#' object_tibble <- function(x) {
#'   as_tibble(x[["object"]])
#' }
#' 
#' #' @importFrom tibble as_tibble
#' #' @importFrom dplyr bind_cols
#' vertex_tibble <- function(x) {
#'   as_tibble(x[["vertex"]])
#' }
#' 
#' #' @importFrom tidygraph activate
#' #' @export activate
#' #' @export
#' activate_.sc <- function(.data, what) {
#'   sc_active(.data) <- what
#'   .data
#' }
#' 
#' sc_active <- function(x) {
#'   attr(x, 'active')
#' }
#' `sc_active<-` <- function(x, value) {
#'   attr(x, 'active') <- value
#'   x
#' }
#' 
#' #' #' @export
#' #' #' @importFrom dplyr filter_
#' #' filter_.sc <- function(.data, ..., .dots) {
#' #'   d_tmp <- as_tibble(.data)
#' #'   if ('.tbl_graph_index' %in% names(d_tmp)) {
#' #'     stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
#' #'   }
#' #'   orig_ind <- seq_len(nrow(d_tmp))
#' #'   d_tmp$.tbl_graph_index <- orig_ind
#' #'   d_tmp <- filter_(d_tmp, ..., .dots = .dots)
#' #'   remove_ind <- orig_ind[-d_tmp$.tbl_graph_index]
#' #'   switch(
#' #'     active(.data),
#' #'     nodes = delete_vertices(.data, remove_ind),
#' #'     edges = delete_edges(.data, remove_ind)
#' #'   ) %gr_attr% .data
#' #' }
#' #' 
#' #' ramp_join <- function (x, ...) {
#' #'   tables <- join_ramp(x)
#' #'   first <- dplyr::filter(x[[tables[1]]], ...)
#' #'   x[[1]] <- last <- first
#' #'   tables <- tables[-1]
#' #'   for (itab in tables) {
#' #'     x[[itab]] <- last <- semi_join(x[[itab]], last)
#' #'   }
#' #'   x
#' #' }