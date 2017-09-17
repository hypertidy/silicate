library(sf)
example(st_read)
library(silicate)
to_leaflet <- function(x, ...) {
  coord <- sc_coord(x)
  path <- sc_path(x)
  object <- sc_object(x)
  object$object_ <- unique(path$object)
  glist <- vector("list", length(unique(object$object_)))
  ind <- rep(seq_len(nrow(path)), path$ncoords_)
  coords_in <- path %>% dplyr::select(-type, -ncol) %>%
    dplyr::slice(ind) %>% dplyr::bind_cols(coord)
  ufeature <- object$object_
  
  for (ifeature in seq_along(ufeature)) {
    gm0 <- path %>% dplyr::filter(object == ufeature[ifeature])
    type <- gm0$type[1]
    coord0 <- coords_in %>% dplyr::filter(object == ufeature[ifeature])
    ## object becomes sub-feature element (not a hole, that is "part")
    coord0$object <- rep(seq_len(nrow(gm0)), gm0$ncoords_)
    glist[[ifeature]] <- switch(type,
                                POINT = as.list(coord0 %>% dplyr::transmute(lat = y_, lng = x_)),
                                MULTIPOINT = as.list(coord0 %>% dplyr::transmute(lat = y_, lng = x_)),
                                LINESTRING = as.list(coord0 %>% dplyr::transmute(lat = y_, lng = x_)),
                                MULTILINESTRING = lapply(split(coord0 %>% dplyr::transmute(lat = y_, lng = x_), coord0$object), as.list),
                                POLYGON = lapply(split(coord0 %>% dplyr::transmute(lat = y_, lng = x_), coord0$object), as.list),
                                MULTIPOLYGON = lapply(split(coord0 %>% dplyr::select(x_, y_, subobject), coord0$object),
                                                                          function(subobject) lapply(split(subobject %>% 
                                    dplyr::transmute(lat = y_, lng = x_), subobject$subobject), as.list)))
  }
 glist 
}

