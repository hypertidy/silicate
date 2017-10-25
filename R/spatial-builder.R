
#' a pattern for building sf objects from 
#' - a gibble, the map of the paths
#' - the coordinates
#' the map is an encoding of the structural properties of the geometry
#' 
#' currently only XY is supported
#' @noRd
build_sf <- function(gm, coords_in, crs = NULL, force_close = FALSE) {
  glist <- vector("list", length(unique(gm$object_)))
  coords_in <- gm %>% dplyr::select(-type, -ncol, -nrow) %>%
    dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>% dplyr::bind_cols(coords_in)
  ufeature <- unique(gm$object_)
  #st <- system.time({
  gmlist <- split(gm, gm$object_)[ufeature]
  coordlist <- split(coords_in, coords_in$object_)[unique(coords_in$object_)]
  #})
  split_to_matrix0 <- if (force_close) split_to_close_matrix else split_to_matrix
  for (ifeature in seq_along(ufeature)) {
  #  gm0 <- gm %>% dplyr::filter(object == ufeature[ifeature])
    gm0 <- gmlist[[ifeature]]
    type <- gm0$type[1]
    coord0 <- coordlist[[ifeature]]
    coord0$path_ <- rep(seq_len(nrow(gm0)), gm0$nrow)
    ## todo need to set XY, XYZ, XYZM, XYM
    cnames <- c("x_", "y_")
    pathnames <- c(cnames, "path_")

    feature <- switch(type,
                                POINT = structure(unlist(coord0[cnames]), class = c("XY", "POINT", "sfg")),
                                MULTIPOINT = structure(as.matrix(coord0[cnames]), class = c("XY", "MULTIPOINT", "sfg")),
                                LINESTRING = structure(as.matrix(coord0[cnames]), class = c("XY", "LINESTRING", "sfg")),
                                MULTILINESTRING = structure(lapply(split(coord0[cnames], coord0[["path_"]]), as.matrix), class = c("XY", "MULTILINESTRING", "sfg")),
                                POLYGON = structure(split_to_matrix0(coord0[cnames], coord0[["path_"]]), class = c("XY", "POLYGON", "sfg")),
                                MULTIPOLYGON = structure(lapply(split(coord0[pathnames], coord0[["subobject"]]),
                                                                          function(path) split_to_matrix0(path[cnames], path[["path_"]])), 
                                                         class = c("XY", "MULTIPOLYGON", "sfg"))
    )
    
    glist[[ifeature]] <- feature
  }
  if (is.null(crs)) {
    crs <- structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")
  } else {
    crs <- switch(mode(crs), 
        character = structure(list(epsg = NA_integer_, proj4string = crs), class = "crs"), 
        numeric = structure(list(epsg = crs, proj4string = NA_character_), class = "crs"))
  }
  bb <- c(range(coords_in[["x_"]]), range(coords_in[["y_"]]))[c(1, 3, 2, 4)]
  names(bb) <- structure(c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
  glist <- structure(glist, class = c(sprintf("sfc_%s", type), "sfc"  ), n_empty = 0, precision = 0, crs = crs, bbox = bb)
  glist
  
}

close_mat = function(m) {
  if (any(m[1, ] != m[nrow(m), ])) 
    rbind(m, m[1, ])
  else m
}
## a fast split
split_to_close_matrix <- function(x, fac) {
  lapply(split(as.vector(t(as.matrix(x))), rep(fac, each = ncol(x))), 
         function(mx) close_mat(matrix(mx, byrow = TRUE, ncol = ncol(x))))
}

split_to_matrix <- function(x, fac) {
  lapply(split(as.vector(t(as.matrix(x))), rep(fac, each = ncol(x))), 
         matrix, byrow = TRUE, ncol = ncol(x))
}


build_sp <- function(gm, coords_in, crs = NULL) {
  glist <- vector("list", length(unique(gm$object_)))
  coords_in <- gm %>% dplyr::select(-type, -ncol, -nrow) %>%
    dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>% dplyr::bind_cols(coords_in)
  ufeature <- unique(gm$object_)
  #st <- system.time({
  gmlist <- split(gm, gm$object_)[ufeature]
  coordlist <- split(coords_in, coords_in$object)[unique(coords_in$object)]
  #})
  
  for (ifeature in seq_along(ufeature)) {
    gm0 <- gmlist[[ifeature]]
    type <- gm0$type[1]
    coord0 <- coordlist[[ifeature]]
    coord0$path_ <- rep(seq_len(nrow(gm0)), gm0$nrow)
    ## todo need to set XY, XYZ, XYZM, XYM
    cnames <- c("x_", "y_")
    pathnames <- c(cnames, "path_")
    
    feature <- switch(type,
                      SpatialPoints = structure(unlist(coord0[cnames]), class = c("XY", "POINT", "sfg")),
                      SpatialMultiPoints = structure(as.matrix(coord0[cnames]), class = c("XY", "MULTIPOINT", "sfg")),
                      SpatialLines = structure(lapply(split(coord0[cnames], coord0[["path_"]]), as.matrix), class = c("XY", "MULTILINESTRING", "sfg")),
                      SpatialPolygons = structure(split_to_matrix(coord0[cnames], coord0[["path_"]]), class = c("XY", "POLYGON", "sfg")))
    
    glist[[ifeature]] <- feature
  }

  glist
  
}
