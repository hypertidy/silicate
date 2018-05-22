path_paste <- function(x, paster = function(...) paste(..., sep = "-")) {
  ## we are looking for  any of these three
  do.call(paster, x[intersect(names(x), c("object", "subobject", "path"))])
}


#' core silicate
#'
#' See https://github.com/hypertidy/silicore for now.
#'
#' @param x an object
#' @param ... reserved for methods
#'
#' @return SC0
#' @export
#'
#' @examples
#' SC0(silicate::minimal_mesh)
SC0 <- function(x, ...) {
  UseMethod("SC0")
}
#' @name SC0
#' @importFrom dplyr mutate slice group_by ungroup select
#' @importFrom gibble gibble
#' @export
SC0.default <- function(x, ...) {
  coord0 <- sc_coord(x)
  ## anything path-based has a gibble (sp, sf, trip at least)
  ## which is just a row-per path with nrow, and object, subobject, path classifiers
  gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
  coord <- coord0 %>% dplyr::mutate(path = as.integer(factor(rep(path_paste(gmap), gmap$nrow))),
                                    vertex = dplyr::row_number()) %>%  dplyr::group_by(path)

  segs <- coord %>% dplyr::select(path, vertex)  %>%
    dplyr::mutate(.vx0 = vertex,   ## specify in segment terms
                  .vx1 = vertex + 1L) %>%
    dplyr::group_by(path)
  segs <- dplyr::slice(segs, -n())
  segs <- segs %>% dplyr::ungroup() %>%
    dplyr::transmute(.vx0, .vx1)


  meta <- tibble::tibble(proj = get_projection(x), ctime = format(Sys.time(), tz = "UTC"))

  structure(list(data = sc_object(x),
                 geometry = gmap,
                 segment = segs,
                 coord = coord0,
                 meta = meta),
            class = c("SC0", "sc"))

}


