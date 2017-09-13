#' silicate.
#'
#' @name silicate
#' @docType package
NULL

#' Minimal mesh. 
#' 
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


#' Inland waters, for parts of Australia. 
#' 
#' The inland waters are lake and so on, presenting as holes
#' within the bounded regions of Australian provinces. 
#' 
#' This is an extract from the old Manifold DVD. It is in `sf` format`.
#' @aliases inlandwaters
#' @name inlandwaters
#' @docType data
NULL


#' Flight tracks
#' 
#' Provided by  Kent Johnson (kent37) in a 
#'  [github discussion](https://github.com/r-spatial/mapview/issues/99#issuecomment-328711275) 
#'  where the data was attached in a zip file.
#' 
#' Original form (in extdata/flight_tracks) is a XYZM LINESTRING shapefile
#' containing 144 flight tracks of aircraft departing runway 33L at Boston Logan
#' airport on June 27, 2017. Data is from an ADS-B recorder. Each point includes
#' lat, lon, altitude in feet and time in North American Eastern Standard Time
#' (EST).
#' 
#' Converted via `sf` into `silicate::PATH` normal form, see (data-raw/flight_tracks.R). 
#' @aliases flight_tracks
#' @name flight_tracks
#' @docType data
NULL

#' Deprecated functions from sc. 
#' 
#' @name sc-deprecated
NULL