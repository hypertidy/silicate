#' silicate.
#'
#' @name silicate
#' @docType package
NULL

#' Polygonal mesh
#'
#' A simple set of `sf` neighbouring polygons, with redundant vertices created
#' from polygonizing a raster.
#' @examples
#' arc <- ARC(polymesh)
#' plot(arc)
#' sc <- SC(polymesh)
#' plot(sc)
#' @name polymesh
#' @docType data
NULL

#' Deprecated data set.
#'
#' This data set is in legacy format and will be removed.
#' A couple of polygons with a single shared edge between them, in
#' PRIMITIVE form.
#' @name mmesh
#' @docType data
NULL


#' Minimal mesh.
#'
#' The simplest pairing of two polygons with one shared edge. One polygon
#' contains a hole and a concavity, the other is a simply convex. This is
#' composed of four "arcs", one around each polygon, one for the shared edge, and
#' one for the isolated hole. There are two nodes, the endpoints of the single shared edge.
#' @examples
#' arc <- ARC(minimal_mesh)
#' plot(arc)
#' sc_arc(arc)
#' sc_node(arc)
#' @name minimal_mesh
#' @docType data
NULL

#' Simple features zoo.
#'
#' Basic examples of each type of simple feature geometry. `sfzoo` is a list
#' with each of *point*, *multipoin*, *linestring*, *multilinestring*, *polygon* and
#' *multipolygon*. `sfgc` is a *GEOMETRYCOLLECTION* of all the types in `sfzoo`.
#' @examples
#' lapply(sfzoo, sc_coord)
#' lapply(sfzoo, sc_path)
#'
#' ## unsure how usefult this is ...
#' sc_path(sfgc)
#' @aliases sfgc sfzoo
#' @name sfzoo
#' @docType data
NULL


#' Inland waters, for parts of Australia, and New Caledonia.
#'
#' The inland waters are lakes and inland waters presenting as holes
#' within the bounded regions of Australian (and New Caledonian) provinces.
#'
#' This is an extract from the old Manifold DVD. It is in `sf` format`.
#' The features have variables `ID` and `Province` they are (in order):
#' \itemize{
#'  \item{"103841"}{Australian Capital Territory}
#'  \item{"103842"}{New Caledonia}
#'  \item{"103843"}{New South Wales}
#'  \item{"103846"}{South Australia}
#'  \item{"103847"}{Tasmania}
#'  \item{"103848"}{Victoria}
#' }
#' There's no good reason that New Caledonia is included and not Queensland (for example)
#' it's just what happened doing a quick crop and extract with the mouse. Lord Howe Island and
#' Macquarie Island are both present, as part of New South Wales and Tasmania respectively.
#' @examples
#' \donttest{
#' path <- PATH(inlandwaters)
#' plot(path)
#' obj <- split(path$path_link_vertex, path$path_link_vertex$path_)
#' cl <- grDevices::colors()[-1L]
#' cols <- sample(cl, length(obj), replace = length(obj) > length(cl))
#' op <- par(mfrow = grDevices::n2mfrow(length(obj)), mar = rep(0, 4))
#' funplot <- function(ob, vert, col) {
#' vx <- c("x_", "y_")
#'   plot(dplyr::inner_join(ob, vert, "vertex_")[vx], col = col, type = "l", axes = FALSE)
#'   }
#' junk <- lapply(seq_along(obj),
#' function(a) {
#'   funplot(obj[[a]], path$vertex, cols[a])
#'   invisible(NULL)
#'   })
#'   par(op)
#'  }
#' @aliases inlandwaters
#' @name inlandwaters
#' @docType data
NULL


#' Flight tracks
#'
#' A data set flight tracks in XYZM form, a form of 4D tracks. Primarily to
#' explore the use of `silicate` as able to represent this topologically,  and to experiment with
#' auto-time-based plotting in `anglr`.
#'
#' Provided by  Kent Johnson (kent37) in a
#'  [github discusion](https://github.com/r-spatial/mapview/issues/99#issuecomment-328711275)
#'  where the data was attached in a zip file.
#'
#' Original form (in extdata/flight_tracks) is a XYZM LINESTRING shapefile
#' containing 144 flight tracks of aircraft departing runway 33L at Boston Logan
#' airport on January 27, 2017. Data is from an ADS-B recorder. Each point includes
#' lat, lon, altitude in feet and time in North American Eastern Standard Time
#' (EST).
#'
#' Converted via `sf` into `silicate::PATH` normal form, see (data-raw/flight_tracks.R).
#'
#' @aliases flight_tracks
#' @name flight_tracks
#' @docType data
NULL

#' Transport routes
#'
#' Routing data set stolen from stplanr
#' see data-raw/routes.R
#' @aliases routes
#' @name routes
#' @docType data
NULL
