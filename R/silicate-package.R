#' silicate.
#'
#' @name silicate
#' @docType package
NULL

#' Polygonal mesh
#' 
#' A simple set of `sf` neighbouring polygons, with redundant vertices created
#' from polygonizing a raster. 
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
#' The simplest pairing of two polygons with one shared edge. 
#' @name minimal_mesh
#' @docType package
NULL

#' Simple features zoo.
#' 
#' A list with basic examples of each type of simple feature. 
#' 
#' \code{sfgc} is a GEOMETRYCOLLECTION, separate from the \code{sfzoo}. 
#' 
#'  The gc contains the zoo.  
#'  @examples 
#' # sf::st_sfc(sfzoo)
#' # sf::st_sfc(sfgc)
#' @aliases sfgc sfzoo
#' @name sfzoo
#' @docType data
NULL


#' Inland waters, for parts of Australia, and New Caledonia. 
#' 
#' The inland waters are lakes and so on, presenting as holes
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
#' it's just what happened doing a quick crop and extract with the mouse. 
#' 
#' @aliases inlandwaters
#' @name inlandwaters
#' @docType data
NULL


#' Flight tracks
#' 
#' An interesting data set in XYZM form, so it can act as a form of 4D tracks. Primarily to
#' explore the use of `silicate` as able to represent this topologically,  and to experiment with
#' auto-time-based plotting in `anglr`. 
#'  
#' Provided by  Kent Johnson (kent37) in a 
#'  [github discussion](https://github.com/r-spatial/mapview/issues/99#issuecomment-328711275) 
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

#' Deprecated functions from silicate. 
#' 
#' `sc_uid(n = )` replaced by `sc_uid(x = )`
#' `PRIMITIVE()`
#' @name sc-deprecated
NULL

#' @inheritParams SC
#' @name sc-deprecated
PRIMITIVE <- function(x, ...) {
  .Defunct("SC", msg = "PRIMITIVE function no longer supported, see `SC` or `TRI` for alternatives")
}
