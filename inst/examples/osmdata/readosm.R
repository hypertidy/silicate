#############################################################
## get some OSM
library(osmdata)
library(xml2)
gda94 <- matrix(rep(c(521494, 5247625), each = 2) + c(-1, 1) * 100, ncol = 2)
ext <- rgdal::project(gda94, "+init=epsg:28355", inv = TRUE)

q <- opq(bbox = as.vector(t(ext)))
#q <- add_osm_feature(q0, key = 'name', value = "Thames", value_exact = FALSE)

encoding <- "UTF-8"
obj <- osmdata()
obj$bbox <- q$bbox
obj$overpass_call <- opq_string(q)
doc <- osmdata:::overpass_query(query = obj$overpass_call, quiet = FALSE,
                                encoding = encoding)

# #saveRDS(doc, file = "inst/examples/osmdata/doc.rds")
# x <- readRDS("inst/examples/osmdata/doc.rds")

#############################################################
## functions to silicify the OSM message
## after parsing out ways (path), nodes (vertex) and
##  pretending (for now) that each way is a feature (object)

library(dplyr)
read_node <- function(x, ...) {
  lst <- xml2::xml_find_all(xml2::read_xml(x), "//node") %>% 
    purrr::map(xml_attrs)
  tibble::as_tibble(do.call(rbind, lst)) %>% 
    dplyr::transmute(vertex_ = id, x_ = as.numeric(lon), y_ = as.numeric(lat))
}
read_way <- function(x, ...) {
  do.call(rbind, xml2::xml_find_all(xml2::read_xml(x), "//way") %>% 
            purrr::map(xml_attrs)) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(path = id)  %>% mutate(object = rev_string(path))
  
}
read_way_ref <- function(x, ...) {
  path <- read_way(x)
#  path <- dplyr::mutate(path, id_ = as.character(seq_len(nrow(path))))
  xml_find_all(xml2::read_xml(x), "//way") %>% 
    map(xml_find_all, "nd") %>% 
    map(function(a) do.call(rbind, xml_attrs(a)) %>% tibble::as_tibble() %>% dplyr::rename(vertex_ = ref)) %>% 
    setNames(path[["path"]]) %>% 
  bind_rows(.id = "path")  
}
rev_string <- function(x) {
  unlist(lapply(strsplit(x, ""), function(y) paste(rev(y), collapse = "")))
}
read_obj <- function(x) {
  read_way(x) %>% dplyr::transmute(object = rev_string(path)) 
}

gibble.PATH <- function(x, ...) {
  inner_join(x[["path"]], x[["path_link_vertex"]] %>% group_by(path) %>% summarize(nrow = n()) ) %>% 
    dplyr::mutate(ncol = 2, type = "MULTILINESTRING")
}

silicate_osm <- function(x) {
  structure( list(object =   read_obj(x),
       path =   read_way(x),
       path_link_vertex = read_way_ref(x), 
       vertex = read_node(x)),
      join_ramp = c("object",           "path",             "path_link_vertex", "vertex"), 
      class = c("PATH", "sc"))
}


#############################################################
## bring in to silicate PATH form
library(gibble)
library(purrr)
library(xml2)
library(silicate)
path_from_osm <- silicate_osm(doc)
dim(path_from_osm$vertex)
## the silicate generics now work, because PATH is a 
## common-form

## note how sc_coord *expands* to the full set of instances
#sc_coord(path_from_osm)
#sc_path(path_from_osm)
#sc_object(path_from_osm)

## gibble is the geometry map, a run-length code of 
## the expanded coordinate set
gm <- gibble(path_from_osm)
library(sf)
sf_geom <- silicate:::build_sf(gm, sc_coord(path_from_osm))
## fakey up a full sf-feature data frame
sf_obj <- st_set_crs(st_sf(geometry = sf_geom, a = unique(gm$object)), 4326)

g <- dismo::gmap(as(sf_obj, "Spatial"), type = "satellite")
library(raster)
plot(g, addfun = function() plot(st_transform(sf_obj, projection(g)), add = T))

###################################################
## convert to PRIMITIVE and provide a summary
prim <- path_from_osm %>% PRIMITIVE()
## no. of segments
nrow(prim$segment)
## no. of unique vertices
nrow(prim$vertex)
## no. of segments per way
prim$segment %>% group_by(path) %>% tally()
## no. of nodes per way
prim$path_link_vertex %>% group_by(path) %>% tally()



# ###################################################
# ## convert to igraph
# library(scgraph)
# sc_as_igraph(prim)
# ## propagate a subset through the graph (tidygraph can do this ...)
# semi_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
#   first <- dplyr::filter(x[[tables[1]]], ...)
#   x[[tables[1]]] <- last <- first 
#   tables <- tables[-1]
#   for (itab in tables) {
#     x[[itab]] <- last <- semi_join_be_quiet_if_there_is_only_1(x[[itab]], last)
#   }
#   x
# }
# p0 <- PATH(prim)
# p0$object <- prim$object[1:4, ]
# semi_cascade(p0, c("object", "path", "path_link_vertex", "vertex"))
# library(ggraph)
# ggraph(sc_as_igraph(prim), layout = 'graphopt') + 
#   geom_edge_link(aes(label = type), 
#                  arrow = arrow(length = unit(4, 'mm')), 
#                  end_cap = circle(3, 'mm')) + 
#   geom_node_point(size = 5)
