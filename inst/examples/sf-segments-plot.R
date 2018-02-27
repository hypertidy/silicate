library(sf)
library(dplyr)
library(p5)
library(silicate)
f <- raadfiles::thelist_files(format = "shp", pattern = "transport_segments")
x <- do.call(rbind, purrr::map(f$fullname, sf::read_sf))
#edge <- silicate::SC(x)
system.time({
  coord <- sc_coord(x)
  path <- sc_path(x)
  path_index <- rep(seq_len(nrow(path)), path$ncoords_)
  p2s <- function(x) cbind(head(x, -1L), tail(x, -1L))
  segs <- do.call(rbind, lapply(split(seq_len(nrow(coord)), path_index), 
                                p2s))
  
  xy1 <- coord[segs[,1], ] %>% 
    transmute(x = x_, y = y_) %>% scaler(nr, nc)
  xy2 <- coord[segs[,2], ] %>% 
    transmute(x = x_, y = y_) %>% scaler(nr, nc)
})
nr <- 350
nc <- 350
exag_fact = 1
plot(range(c(xy1$x, xy2$x)), range(c(xy1$y, xy2$y)))
system.time(segments(xy1$x, xy1$y, xy2$x, xy2$y))

dim(xy1)
pryr::object_size(list(xy1, xy2))
#p5() %>% createCanvas(nr * exag_fact, nc * exag_fact, renderer = NULL) %>% 
#  line(x1 = segs[,1], y1 = segs[,3], x2 = segs[,2], y2 = segs[,4])


