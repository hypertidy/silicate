# sort(unique(unlist(lapply(strsplit(grep("sc_", ls("package:silicate"), value = TRUE), "\\."), "[", 1))))
# [1] "sc_arc"          "sc_arc_base"     "sc_arc_PATH"     "sc_atom"         "sc_colours"      "sc_compact"     
# [7] "sc_coord"        "sc_edge"         "sc_expand"       "sc_geom_names"   "sc_list"         "sc_node"        
# [13] "sc_node_base"    "sc_object"       "sc_path"         "sc_segment"      "sc_segment_base" "sc_uid"         
# [19] "sc_vertex"  
# 
# 
# lfuns <- list(sc_arc, sc_coord, sc_edge, sc_node, sc_object, sc_path, sc_segment,  sc_vertex)
# 
# lfuns %>% invoke_map(, minimal_mesh) %>% purrr::map_int(nrow)
# lfuns[-5] %>% invoke_map(, PATH(minimal_mesh)) %>% purrr::map_int(nrow)
# lfuns[-c(5, 6, 7)] %>% invoke_map(, SC(minimal_mesh))

na_split <- function(x) {
  x <- split(x[c("x_", "y_")], x$path_)
  if (length(x) == 1) x[[1]] else head(dplyr::bind_rows(lapply(x, function(x) rbind(dplyr::distinct(x), NA))), -1)
}

#x <- PATH(minimal_mesh)
triangulate.PATH <- function(x, ...) {
  objlist <- x$path %>% split(.$object_)
  objlist <- objlist[unique(x$path$object_)]
  trilist <- setNames(vector("list", length(objlist)), names(objlist))
  for (i in seq_along(objlist)) {
    verts <- objlist[[i]] %>% dplyr::select(.data$object_, .data$path_) %>% 
      dplyr::inner_join(x$path[c("path_", "object_")], "path_") %>% 
      dplyr::select(.data$path_) %>% 
      dplyr::inner_join(x$path_link_vertex, "path_") %>% 
      dplyr::inner_join(x$vertex, "vertex_")
    trimat <- na_split(verts) %>% earclip.rgl::earclip_rgl()
    #trimat <- try(na_split(verts) %>% earclip.rgl::earclip_rgl(), silent = TRUE)
    #if (inherits(trimat, "try-error")) {
    #  trilist[[i]] <- NULL 
    #  } else {
    trilist[[i]] <- tibble::tibble(.vertex0 = verts$vertex_[trimat[1, ]], 
                   .vertex1 = verts$vertex_[trimat[2, ]], 
                   .vertex2 = verts$vertex_[trimat[3, ]], 
                   triangle_ = sc_uid(ncol(trimat)))
     # }
  }
  dplyr::bind_rows(trilist, .id = "object_")
}
TRI <- function(x, ...) {
  UseMethod("TRI")
}
TRI.default <- function(x, ...) {
  TRI(PATH(x, ...))
}
TRI.PATH <- function(x, ...) {
  if(any(grepl("MULTIPOLYGON", x$path$type))) {
    message("MULTIPOLYGON ear clipping doesn't work in some cases:")
    message("* try `sf::st_cast(x, \"POLYGON\")` if it fails")
  }
  tri <- triangulate.PATH(x)
  obj <- sc_object(x)
  #obj <- obj[obj$object_ %in% tri$object_, ]
  structure(list(object = obj, triangle = tri, 
                 vertex = sc_vertex(x)), class = c("TRI", "sc"))
}
sc_object.TRI <- function(x, ...) {
  x[["object"]]
}
plot.TRI <- function(x, ...) {
  
  plot(x$vertex[c("x_", "y_")])
  cols <- sc_colours(nrow(sc_object(x)))
  purrr::iwalk(sc_object(x)$object_, 
              ~dplyr::filter(x$triangle, .data$object_ == .x) %>% 
                dplyr::transmute(.vertex0, .vertex1, .vertex2, fill = NA_character_) %>% 
               t() %>% 
               as.vector() %>% 
               tibble::tibble(vertex_ = .) %>% 
                dplyr::left_join(x$vertex, "vertex_") %>% dplyr::select(x_, y_) %>% 
                dplyr::slice(-n()) %>% 
               polypath(col = cols[.y]))
}
#plot(TRI(minimal_mesh))


