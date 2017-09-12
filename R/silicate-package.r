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
#' Taken from  Kent Johnson (kent37) in a 
#'  [github discussion](https://github.com/r-spatial/mapview/issues/99#issuecomment-328711275).
#' 
#' Original form is a shapefile containing 144 flight tracks of aircraft departing
#'  runway 33L at Boston Logan airport on June 28, 2017. Data is from an ADS-B 
#'  recorder so it is kind of rough. Each point includes lat, lon, altitude in feet
#'  and time in EDT. 
#' 
#' Converted via `sf` into `silicate::PATH` normal form. 
#' @aliases flight_tracks
#' @name flight_tracks
#' @docType data
NULL

#' Deprecated functions from sc. 
#' 
#' @name sc-deprecated
NULL