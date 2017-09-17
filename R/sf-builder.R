#' a pattern for building sf objects from 
#' - a gibble, the map of the paths
#' - the coordinates
#' the map is an encoding of the structural 
build_sf <- function(gm, coords_in, crs = NULL) {
  glist <- vector("list", length(unique(gm$object)))
  coords_in <- gm %>% dplyr::select(-type, -ncol, -nrow) %>%
    dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>% dplyr::bind_cols(coords_in)
  ufeature <- unique(gm$object)
  #st <- system.time({
  gmlist <- split(gm, gm$object)[ufeature]
  coordlist <- split(coords_in, coords_in$object)[unique(coords_in$object)]
  #})

  for (ifeature in seq_along(ufeature)) {
  #  gm0 <- gm %>% dplyr::filter(object == ufeature[ifeature])
    gm0 <- gmlist[[ifeature]]
    type <- gm0$type[1]
    coord0 <- coordlist[[ifeature]]
#    coord0 <- coords_in %>% dplyr::filter(object == ifeature)
    ## object becomes sub-feature element (not a hole, that is "part")
    coord0$path <- rep(seq_len(nrow(gm0)), gm0$nrow)
    ## todo need to set XY, XYZ, XYZM, XYM
    feature <- switch(type,
                                POINT = structure(unlist(coord0 %>% dplyr::select(x_, y_)), class = c("XY", "POINT", "sfg")),
                                MULTIPOINT = structure(as.matrix(coord0 %>% dplyr::select(x_, y_)), class = c("XY", "MULTIPOINT", "sfg")),
                                LINESTRING = structure(as.matrix(coord0 %>% dplyr::select(x_, y_)), class = c("XY", "LINESTRING", "sfg")),
                                MULTILINESTRING = structure(lapply(split(coord0 %>% dplyr::select(x_, y_), coord0$path), as.matrix), class = c("XY", "MULTILINESTRING", "sfg")),
                                POLYGON = structure(split_to_matrix(coord0 %>% dplyr::select(x_, y_), coord0$path), class = c("XY", "POLYGON", "sfg")),
                                MULTIPOLYGON = structure(lapply(split(coord0 %>% dplyr::select(x_, y_, path), coord0$subobject),
                                                                          function(path) split_to_matrix(path %>% select(x_, y_), path$path)), 
                                                         class = c("XY", "MULTIPOLYGON", "sfg"))
    )
    
    glist[[ifeature]] <- feature
  }
  if (is.null(crs)) crs <- NA_crs_
  #sfd[["geometry"]] <- structure(ol, class = c("sfc_MULTIPOLYGON", "sfc"  ), n_empty = 0, precision = 0, crs = na_crs, bbox = bb)
  #structure(sfd, sf_column = "geometry", agr = factor(NA, c("constant", "aggregate", "identity")), class = c("sf", class(sfd)))
  
  out <-   st_sfc(glist, crs = crs)
  out
  
}


## a fast split
split_to_matrix <- function(x, fac) {
  lapply(split(as.vector(t(as.matrix(x))), rep(fac, each = ncol(x))), 
         matrix, byrow = TRUE, ncol = ncol(x))
}


