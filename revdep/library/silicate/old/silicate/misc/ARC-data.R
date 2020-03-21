v <- sf::read_sf(raadfiles::thelist_files(pattern = "transport_segments_hobart")$fullname[1])

library(silicate)
n <- 2697
sfx <- v[sort(unique(c(n, unlist(sf::st_touches(sf::st_geometry(v)[n], sf::st_geometry(v)))))), ]

sfy <- spex::polygonize(raster::raster(matrix(1:2, ncol = 2)))

library(sf)
sc <- SC0(sfx)
plot(sc, col = rgb(0, 0, 0, 0.5), lwd = 15, asp = 1)
plot(sfx[1], lwd = 3, reset = FALSE)
text(sc$vertex, lab = 1:nrow(sc$vertex), pos = 1:4, cex = 0.8, offset = 0.3)



## on creation, SC0 has all edge instances in the order they occur
sc$object$object_ <- seq_len(nrow(sc$object))
edge <- tidyr::unnest(sc$object[c("object_", "topology_")])


v_0 <- pmin(edge$.vx0, edge$.vx1)
v_1 <- pmax(edge$.vx0, edge$.vx1)
edge$native_ <- v_0 == edge$.vx0  ## if TRUE the orientation is how it came in

uedge <- edge
## we now have ordered edges
uedge[[".uvx0"]] <- v_0
uedge[[".uvx1"]] <- v_1

uedge[["u_edge"]] <- dplyr::group_indices(uedge, .data$.uvx0, .data$.uvx1)


uedge %>% group_by(.uvx0, .uvx1) %>% tally()  %>% print(n = Inf)
## nodes that occur twice or more
shared_verts <- uedge$.uvx0[which(!match(uedge$.uvx1, uedge$.uvx0) == (seq_len(nrow(uedge)) - 1))]


## nodes that occur only once (for lines)
terminal_verts <- tibble::tibble(vertex_ = as.vector(t(as.matrix(edge[c(".vx0", ".vx1")])))) %>%
  dplyr::count(.data$vertex_) %>% dplyr::filter(n == 1) %>% dplyr::pull(.data$vertex_)

plot(sfx[1], lwd = c(10, 2), reset = FALSE)
if (length(shared_verts) > 0) points(sc$vertex[shared_verts, c("x_", "y_")], col = "red", cex = 1.5)
if (length(terminal_verts) > 0) points(sc$vertex[terminal_verts, c("x_", "y_")], col = "green", cex = 1, pch = 19)

points(sc$vertex[c("x_", "y_")], pch = sample(1:23), cex = c(1:3))
