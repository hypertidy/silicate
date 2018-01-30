td <- "~"
u = "http://download.geofabrik.de/europe/albania-latest.osm.pbf"
f <- normalizePath(file.path(td, basename(u)))
#download.file(u, f, mode = "wb")
x <- vapour::vapour_read_geometry(f, 4)
#f <- list.files(basename(tempdir()), recursive = TRUE, pattern = "pbf$")
read_pbf_geometry <- function(file) {
  layers <- sf::st_layers(file)$name
  setNames(purrr::map(layers, 
                      ~sf::read_sf(file, .x)), layers)
}
x <- read_pbf_geometry(f)

purrr::map_int(x, nrow)
purrr::map_int(x, ncol)



download.file(u, f, mode = "wb")
msg = "osmconvert albania.osh.pbf >albania.osm")
system(msg)
osmdata_xml(input_file = "albania.osh.pbf", filename = "albania.osm")
q = add_feature(opq("Albania"), "highway")
albanian_roads = osmdata_sf(q, doc = "albania.osm")