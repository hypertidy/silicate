sfzoo <- function() {
  
  x <- st_point(c(1,2))
  
  p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
  mp <- st_multipoint(p)
  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  ls <- st_linestring(s1)
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))
  mls <- st_multilinestring(list(s1,s2,s3))
  p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
  p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
  pol <-st_polygon(list(p1,p2))
  p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
  p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
  p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
  mpol <- st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5)))
  #gc <- st_geometrycollection(list(mp, mpol, ls))
  
  st_sfc(list(point = x, multipoint = mp, linestring = ls, multilinestring = mls, polygon = pol,
       multipolygon = mpol))
}
sfgeomc <- function() {
  gc <- st_geometrycollection(sfzoo())
}

branch <- function(x) UseMethod("branch")
#' @name branch
#' @examples 
#' zoo <- sfzoo()
#' branch(zoo)
#' branch(hpoly)
branch.sfc <- function(x) bind_rows(unname(lapply(x, sc_branch)), .id = "object_")
branch.sf <- function(x) branch(st_geometry(x))


m_v <- function(x) {
  x <- if (is.null(dim(x))) t(x) else x
}
sfcoords <- function(x, ...) tibble::as_tibble(m_v(x))
sc_coord <- function(x, ...) UseMethod("sc_coord")
sc_coord.MULTIPOLYGON <- function(x, ...) {
  dplyr::bind_rows(lapply(x, function(y) bind_rows(lapply(y, sfcoords))))
}

BRANCH  <- function(x) {
  b <- sc_branch(x)
  v <- sc_coord(x)
  bXv <- tibble(branch_ = rep(b$branch_, b$ncoords), vertex_ = sc_rand(n = nrow(v)))
  v[["branch_"]] <- bXv[["branch_"]]
  list(b = b, v = v, bXv = bXv)
}

sc_rand <- function(n = 1L) ids::random_id(n, bytes = 8)
sc_atom <- function(x) tibble::tibble(ncoords = nrow(x), branch_ = sc_rand())
sc_list <- function(x) dplyr::bind_rows(lapply(x, sc_atom))
#' @name sc_branch
#' @examples 
#' zoo <- sfzoo()
#' lapply(zoo, sc_branch)
sc_branch <- function(x, ...) UseMethod("sc_branch")
#' @name sc_branch
#' @examples 
#' 
#' sc_branch(zoo$multipolygon)
sc_branch.MULTIPOLYGON <- function(x, ...) {
  dplyr::bind_rows(lapply(x, sc_list), .id = "island_")
}
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$polygon)
sc_branch.POLYGON <- function(x, ...) {
  sc_list(x)
}
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$linestring)
sc_branch.LINESTRING <- sc_atom
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$multilinestring)
sc_branch.MULTILINESTRING <- sc_branch.POLYGON
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$point)
sc_branch.POINT <- function(x) sc_atom(matrix(x, nrow = 1L))
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$multipoint)
sc_branch.MULTIPOINT <- function(x) tibble(ncoords = 1, branch = sc_rand(n = nrow(x)))
#' @name sc_branch
#' @examples 
#' sc_branch(zoo$multipoint)
sc_branch.GEOMETRYCOLLECTION <- function(x) dplyr::bind_rows(lapply(x, sc_branch), .id = "collection_")


## infix sugar for if (is.null)
"%||%" <- function(a, b) {
  if (is.null(a)) b else a
}
