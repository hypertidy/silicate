# Let's have a crack at topological simplification. 

library(silicate)
library(sf)
x <- inlandwaters[c(3, 6), ]
plot(x[1])

sc_arc(x)

remove_pc <- function(x, pc = 0.5) {
  ## keep the first and last
  idx <- seq(2, nrow(x) - 1)
  if (nrow(x) == 2) return(x)
  idx <- seq(2, nrow(x) - 1, length = nrow(x) * pc)
  x[c(1, idx, nrow(x)), ]
}

approx_pc <- function(x, pc = 0.1) {
  ii <- seq_len(nrow(x))
  xf <- approxfun(ii, x$x_)
  yf <- approxfun(ii, x$y_)
  idx <- seq(1, nrow(x), length = nrow(x) * pc)
  tibble::tibble(x_ = xf(idx), y_ = yf(idx))
}

length_weight_pc <- function(x, pc = 0.1) {
  lengs <- sp::spDists(as.matrix(x), segment = TRUE)
  ord <- rev(order(lengs[-c(1, length(lengs))])) + 1
  minlen <- 3
  len <- length(lengs) * pc
  idx <- c(1, sort(ord[seq_len(pmax(minlen, len))]), length(lengs))
  x[idx, ]
}
a_rea <- function(x) {
  (x[1, 1] - x[2, 1])^2 * (x[1, 2] - x[2, 2])^2
}
eff_area <- function(x) {
  unlist(lapply(split(as.vector(t(head(matrix(seq_len(nrow(x) - 1), nrow(x), 3), -3))), 
        rep(1:(nrow(x)-3), each= 3)), 
        function(a) a_rea(x[a, ])/2))
}
vis <- function(x, pc = 0.1) {
  area <- eff_area(as.matrix(x))
  ord <- order(area)
  ord <- ord[ord > (pc *nrow(x))]
  x[ord]
}
## we also need to rebuild from arcs
arc_sf <- function(arc, v, pc = 0.1) {
  if (!pc > 0) stop("pc must be > 0 (and < 1)")
  if (!pc < 1) stop("pc must be < 1 (and > 0)")
sf::st_sfc(  arc %>% split(.$arc_) %>% 
    purrr::map(~inner_join(.x, v, "vertex_") %>% 
                 dplyr::select(x_, y_) %>% 
                # remove_pc(pc = pc) %>%
                 #length_weight_pc(pc = pc) %>%
                 vis(pc = pc) %>% 
                 as.matrix() %>% 
                 sf::st_linestring())  
)
}
example(st_read)
x <- nc
x <- inlandwaters
p <- PATH(x)

## original line string
lstr <- st_cast(st_geometry(x), "MULTILINESTRING")
## simplified line string
slstr <- arc_sf(sc_arc(p), p$v, pc = 0.5)
st_crs(slstr) <- st_crs(lstr)
plot(slstr, col = viridis::viridis(20))
pryr::object_size(lstr)
pryr::object_size(slstr)
