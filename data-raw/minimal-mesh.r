p1 <- cbind(x = c(0, 0, 0.75, 1,   0.5, 0.8, 0.69, 0), 
            y = c(0, 1, 1,    0.8, 0.7, 0.6, 0,    0))
p2 <- cbind(x = c(0.2, 0.2, 0.3, 0.5, 0.5, 0.2), 
            y = c(0.2, 0.4, 0.6, 0.4, 0.2, 0.2))
p4 <- cbind(x = c(0.69, 0.8, 1.1, 1.23, 0.69), 
            y = c(0, 0.6, 0.63, 0.3, 0))
pp <- rbind(p1, NA,  p2[nrow(p2):1, ])
#plot(rbind(pp, p4), cex = 1.3, main = "two polygons, shared edge")
#polypath(pp, col = "grey")
#polypath(p4, col = "firebrick")

library(sf)
minimal_mesh <- st_sf(a = 1:2, 
                         geom = st_sfc(list(st_multipolygon(list(list(p1, p2[rev(seq(nrow(p2))), ]))),  
                                                 st_multipolygon(list(list(p4))))))
#plot(x, col = c("grey", "firebrick"))
devtools::use_data(minimal_mesh)
