f <- "https://github.com/r-gris/polyggon/raw/master/inst/extdata/inlandwaters.gpkg"
lfile <- file.path("data-raw", basename(f))
if (!file.exists(lfile)){
download.file(f, lfile, mode = "wb")
}

library(sf)
library(dplyr)
inlandwaters <- st_read(file.path("data-raw", basename(f)), 
                        stringsAsFactors = FALSE) %>% 
  slice(c(1, 2, 3, 6, 7, 8))
plot(inlandwaters)

devtools::use_data(inlandwaters, compress = "xz")
