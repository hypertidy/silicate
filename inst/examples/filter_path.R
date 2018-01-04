# select.PATH <- function(x, ...) {
#   x$object <- dpylr::select(x$object, ...)
#   x
# }

filter.PATH <- function(x, ...) {
  x$object <- dplyr::filter(x$object, ...)
  anglr:::semi_cascade(x, tables = c("object", "path", "path_link_vertex", "vertex"))
}
filter.PATH(p, AREA > 0.124)
library(ggplot2)
library(dplyr)
plot.PATH <- function(x, ...)  {
  ggplot(purrr::reduce(x[c("object", "path", "path_link_vertex", "vertex")], inner_join)) + 
    geom_polygon(aes(x_, y_, fill = object_, group = path_))  + guides(colour = FALSE, fill = FALSE)
}
plot(filter(g, PERIMETER > 1.5))


library(dplyr)
read_shp <- function(pattern, ext = NULL) {
  out <- sf::read_sf(raadfiles::thelist_files(format = "shp", pattern = pattern) %>% 
                pull(fullname)) #%>% silicate::PATH()
  if (!is.null(ext)) {
    out <- st_intersection(out, ext)
  }
  out
}
#ex <- drawExtent()
ex <- st_as_sf(as(ex, "SpatialPolygons"))
st_crs(ex) <- st_crs("+proj=utm +zone=55 +south +ellps=GRS80 +units=m +no_defs")
parcels <- read_shp("parcels_hobart", ex)
contours <- read_shp("5m_contours_hobart", ex)

parcels <- st_cast(parcels, "MULTIPOLYGON")
library(anglr)
a <- anglr(parcels, z = "MEAS_AREA")
b <- anglr(contours, z = "ELEVATION")

