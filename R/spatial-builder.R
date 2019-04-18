
# a pattern for building sf objects from
# - a gibble, the map of the paths
# - the coordinates
# the map is an encoding of the structural properties of the geometry
# currently only XY is supported
build_sf <- function(x, ...) {
  UseMethod("build_sf")
}
build_sf.PATH <- function(x, ...) {
  out <- x$object
  agr <- factor(setNames(rep(NA_integer_, ncol(out)), names(out)))
  out[["geometry"]] <- build_sfc(x)
  attr(out, "sf_column") <- "geometry"
  attr(out, "agr") <- agr
  class(out) <- c("sf", class(out))
  out
}

build_sfc <- function(x, ...) {
  UseMethod("build_sfc")
}
build_sfc.PATH <- function(x, ...) {
  raw_build_sfc(x$path, x$path_link_vertex %>% dplyr::inner_join(x$vertex, "vertex_"),
               crs = x$meta$proj[1])
}
raw_build_sfc <- function(gm, coords_in, crs = NULL, force_close = FALSE) {
  if (!"object_" %in% names(gm)) gm$object_ <- gm$object
  if (!"subobject" %in% names(gm)) gm$subobject <- 1
  if (!"nrow" %in% names(gm)) gm$nrow = gm$ncoords_
  glist <- vector("list", length(unique(gm$object_)))
  coords_in <- gm %>% dplyr::select(.data$object_, .data$subobject) %>%
    #dplyr::select(-type, -ncol, -nrow) %>%
    dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>%
    dplyr::bind_cols(coords_in)
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
  glist <- structure(glist, precision = 0, bbox = bb, crs = crs, n_empty = 0,  crs = crs,
                     class = c(sprintf("sfc_%s", type), "sfc"  ))
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


# build_sp <- function(gm, coords_in, crs = NULL) {
#   glist <- vector("list", length(unique(gm$object)))
#   coords_in <- gm %>% dplyr::select(-type, -ncol, -nrow) %>%
#     dplyr::slice(rep(seq_len(nrow(gm)), gm$nrow)) %>% dplyr::bind_cols(coords_in)
#   ufeature <- unique(gm$object)
#   #st <- system.time({
#   gmlist <- split(gm, gm$object)[ufeature]
#   coordlist <- split(coords_in, coords_in$object)[unique(coords_in$object)]
#   #})
#
#   for (ifeature in seq_along(ufeature)) {
#     gm0 <- gmlist[[ifeature]]
#     type <- gm0$type[1]
#     coord0 <- coordlist[[ifeature]]
#     coord0$path_ <- rep(seq_len(nrow(gm0)), gm0$nrow)
#     ## todo need to set XY, XYZ, XYZM, XYM
#     cnames <- c("x_", "y_")
#     pathnames <- c(cnames, "path_")
#
#     feature <- switch(type,
#                       SpatialPoints = structure(unlist(coord0[cnames]), class = c("XY", "POINT", "sfg")),
#                       SpatialMultiPoints = structure(as.matrix(coord0[cnames]), class = c("XY", "MULTIPOINT", "sfg")),
#                       SpatialLines = structure(lapply(split(coord0[cnames], coord0[["path_"]]), as.matrix), class = c("XY", "MULTILINESTRING", "sfg")),
#                       SpatialPolygons = structure(split_to_matrix(coord0[cnames], coord0[["path_"]]), class = c("XY", "POLYGON", "sfg")))
#
#     glist[[ifeature]] <- feature
#   }
#
#   glist
#
# }
#


## methods for reconstruction from SC
## https://github.com/hypertidy/silicate/issues/81#issuecomment-435833446

# to_dodgr <- function(x, ...) {
#   UseMethod("to_dodgr")
# }
# to_dodgr.SC <- function(x, ...) {
#   edge <- x$object_link_edge %>% dplyr::inner_join(x$edge, "edge_")
#   edge1 <- edge %>% dplyr::filter(native_)
#   edge2 <- edge %>% dplyr::filter(!native_)
#   tmp <- edge2$.vx0
#   edge2$.vx0 <- edge2$.vx1
#   edge2$.vx1 <- tmp
#   edge <- dplyr::bind_rows(edge1, edge2) %>% dplyr::transmute(from_id = .vx0, to_id = .vx1, edge_)
#   edge %>% dplyr::mutate(edge_id = row_number(), d = 1)
# #  edge <- x$edge %>% dplyr::transmute(from_id = .vx0, to_id = .vx1, edge_)
# #  edge2 <- tibble::tibble(from_id = edge$to_id, to_id  = edge$from_id, edge_ = edge$edge_)
# #  dplyr::bind_rows(edge, edge2) %>% dplyr::mutate(edge_id = row_number(), d = 1)
# }
#
# expand_verts <- function(a) {
#   a %>% inner_join(sc$vertex, c("from_id" = "vertex_")) %>% dplyr::rename(x0 = x_, y0 = y_) %>%
#     inner_join(sc$vertex, c("to_id" = "vertex_"))
# }
#
# to_arcs.SC <- function(x, ...) {
#   instances <- to_dodgr(x)
#   ## this can't work this way, because single edges and paired edges are left out
#   # https://github.com/ATFutures/dodgr/issues/78#
#   cg <- dodgr::dodgr_contract_graph(instances)
#
#
#   als <- cg$edge_map  %>%  split(.$edge_new) %>% purrr::map(~
#                                                             dplyr::inner_join(.x, instances %>% mutate(edge_id = as.character(edge_id)), c("edge_old" = "edge_id")) )
#   als
# }
