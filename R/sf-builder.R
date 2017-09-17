#' a pattern for building sf objects from 
#' - a gibble, the map of the paths
#' - the coordinates
#' the map is an encoding of the structural 
build_sf <- function(gm, coords_in, crs = NULL) {
  glist <- vector("list", length(unique(gm$object)))
  coords_in <- gm %>% dplyr::select(-type, -ncol, -nrow) %>%
    dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>% dplyr::bind_cols(coords_in)
  ufeature <- unique(gm$object)
  for (ifeature in seq_along(ufeature)) {
    gm0 <- gm %>% dplyr::filter(object == ufeature[ifeature])
    type <- gm0$type[1]
    coord0 <- coords_in %>% dplyr::filter(object == ifeature)
    ## object becomes sub-feature element (not a hole, that is "part")
    coord0$path <- rep(seq_len(nrow(gm0)), gm0$nrow)
    feature <- switch(type,
                                POINT = sf::st_point(unlist(coord0 %>% dplyr::select(x_, y_))),
                                MULTIPOINT = sf::st_multipoint(as.matrix(coord0 %>% dplyr::select(x_, y_))),
                                LINESTRING = sf::st_linestring(as.matrix(coord0 %>% dplyr::select(x_, y_))),
                                MULTILINESTRING = sf::st_multilinestring(lapply(split(coord0 %>% dplyr::select(x_, y_), coord0$path), as.matrix)),
                                POLYGON = sf::st_polygon(lapply(split(coord0 %>% dplyr::select(x_, y_), coord0$path), as.matrix)),
                                MULTIPOLYGON = structure(lapply(split(coord0 %>% dplyr::select(x_, y_, path), coord0$subobject),
                                                                          function(path) lapply(split(path %>% select(x_, y_), path$path), as.matrix)), 
                                                         class = c("XY", "MULTIPOLYGON", "sfg")
                                )
    )
    
    glist[[ifeature]] <- feature
  }
  if (is.null(crs)) crs <- NA_crs_
  out <-   st_sfc(glist, crs = crs)
  out
  
}



split_to_matrix <- function(x, fac) {
  lapply(split(as.vector(t(as.matrix(x))), rep(fac, each = ncol(x))), 
         matrix, byrow = TRUE, ncol = ncol(x))
}


