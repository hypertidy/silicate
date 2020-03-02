library(stplanr)
library(sf)
routes <- routes_fast_sf[as.numeric(st_length(routes_fast_sf)) > 0, "ID"]
class(routes$geometry) <- c("sfc_LINESTRING", "sfc" , "list")
usethis::use_data(routes, overwrite = TRUE)
