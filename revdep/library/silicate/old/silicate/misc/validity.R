tri_fix <- function(x) {
  sf::st_cast(sfdct::ct_triangulate(x)) %>% sf::st_is_valid()
}

sil_fix <- function(x) {
  sc <- silicate::SC(x)
  seg <- silicate::sc_segment(x)
  
}
#A 'bowtie' polygon:
  
bowtie <-   "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
x <- sf::st_as_sfc(bowtie) 
sf::st_is_valid(x)
tri_fix(x)

#' Square with wrong orientation:
  
sq_wo <-   "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"
x <- sf::st_as_sfc(sq_wo) 
sf::st_is_valid(x)
tri_fix(x)

#' Inner ring with one edge sharing part of an edge of the outer ring:
ir_share  <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0),(5 2,5 7,10 7, 10 2, 5 2))"
x <- sf::st_as_sfc(ir_share) 
sf::st_is_valid(x)
tri_fix(x)


#' Dangling edge:
  
de <- "POLYGON((0 0, 10 0, 15 5, 10 0, 10 10, 0 10, 0 0))"
x <- sf::st_as_sfc(de) 
sf::st_is_valid(x)
tri_fix(x)

#' Outer ring not closed:
  
ornc <-   "POLYGON((0 0, 10 0, 10 10, 0 10))"
x <- sf::st_as_sfc(ornc) 
sf::st_is_valid(x)
tri_fix(x)

## sfdct currently can't fix this one, but
sc <- silicate::SC(x)

p <- RTriangle::pslg(S = matrix(match(as.matrix(sc$edge %>% dplyr::select(.vertex0, .vertex1)), sc$vertex$vertex_), ncol = 2), 
                P = as.matrix(sc$vertex %>% dplyr::select(x_, y_)))
tr <- RTriangle::triangulate(p, a = 1)
purrr::transpose(sc$edge) %>% 
  purrr::map(~tibble::tibble(vertex_ = unlist(.x[c(".vertex0", ".vertex1")])) %>% 
               dplyr::inner_join(sc$vertex))

#Two adjacent inner rings:
  
tr <-   "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 8, 3 8, 3 1, 1 1), (3 1, 3 8, 5 8, 5 1, 3 1))"
x <- sf::st_as_sfc(tr) 
sf::st_is_valid(x)
tri_fix(x)

#Polygon with an inner ring inside another inner ring:
  
pr <-   "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (2 8, 5 8, 5 2, 2 2, 2 8), (3 3, 4 3, 3 4, 3 3))"
x <- sf::st_as_sfc(pr) 
sf::st_is_valid(x)
tri_fix(x)

