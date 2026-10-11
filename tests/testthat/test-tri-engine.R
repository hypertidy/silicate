tri_area <- function(t) {
  v <- t$vertex
  i0 <- match(t$triangle$.vx0, v$vertex_)
  i1 <- match(t$triangle$.vx1, v$vertex_)
  i2 <- match(t$triangle$.vx2, v$vertex_)
  0.5 * ((v$x_[i1] - v$x_[i0]) * (v$y_[i2] - v$y_[i0]) -
           (v$x_[i2] - v$x_[i0]) * (v$y_[i1] - v$y_[i0]))
}
engines <- function() {
  if (!requireNamespace("meshcore", quietly = TRUE)) return(character(0))
  c("cdtr", "laridae")[c(requireNamespace("cdtr", quietly = TRUE),
                         requireNamespace("laridae", quietly = TRUE))]
}

test_that("engine TRI covers the same area as earcut, CCW, per object", {
  skip_if(length(engines()) == 0L, "no triangulation engine installed")
  ear <- TRI(minimal_mesh, engine = "earcut")
  ear_area <- sum(abs(tri_area(ear)))
  for (e in engines()) {
    tri <- TRI(minimal_mesh, engine = e)
    expect_s3_class(tri, "TRI")
    a <- tri_area(tri)
    expect_true(all(a > 0))
    expect_equal(sum(a), ear_area)
    expect_equal(sort(unname(tapply(a, tri$triangle$object_, sum))),
                 sort(unname(tapply(abs(tri_area(ear)), ear$triangle$object_, sum))))
    expect_true(all(c(tri$triangle$.vx0, tri$triangle$.vx1, tri$triangle$.vx2) %in% tri$vertex$vertex_))
    expect_true(all(tri$triangle$visible))
  }
})

test_that("refinement adds Steiner vertices and keeps the area", {
  skip_if(length(engines()) == 0L, "no triangulation engine installed")
  ear_area <- sum(abs(tri_area(TRI(minimal_mesh, engine = "earcut"))))
  for (e in engines()) {
    tri <- TRI(minimal_mesh, engine = e, max_area = 0.01, min_angle = 20)
    a <- tri_area(tri)
    expect_equal(sum(a), ear_area)
    expect_true(max(a) <= 0.01 * (1 + 1e-9))
    expect_gt(nrow(tri$vertex), nrow(sc_vertex(PATH(minimal_mesh))))
    expect_false(anyDuplicated(tri$vertex$vertex_) > 0)
  }
})

test_that("vertex attributes are interpolated onto new vertices", {
  skip_if(length(engines()) == 0L, "no triangulation engine installed")
  x <- c(0, 4, 4, 0, 0); y <- c(0, 0, 3, 3, 0)
  p <- PATH(minimal_mesh)
  ## a single square with z = x + 2 y
  p$vertex <- tibble::tibble(x_ = x[-5], y_ = y[-5], z_ = x[-5] + 2 * y[-5],
                             vertex_ = p$vertex$vertex_[1:4])
  p$path <- p$path[1, ]
  p$path$ncoords_ <- 5L
  p$path_link_vertex <- tibble::tibble(path_ = p$path$path_,
                                       vertex_ = p$vertex$vertex_[c(1:4, 1)])
  p$object <- p$object[p$object$object_ %in% p$path$object_, ]
  for (e in engines()) {
    tri <- TRI(p, engine = e, max_area = 0.5)
    v <- tri$vertex
    expect_gt(nrow(v), 4L)
    expect_equal(v$z_, v$x_ + 2 * v$y_, tolerance = 1e-9)
    expect_equal(sum(tri_area(tri)), 12)
  }
})

test_that("auto picks an engine and earcut is always available", {
  expect_true(silicate:::tri_engine("auto") %in% c("cdtr", "earcut"))
  expect_identical(silicate:::tri_engine("earcut"), "earcut")
  old <- options(silicate.tri_engine = "earcut")
  on.exit(options(old))
  expect_s3_class(TRI(minimal_mesh), "TRI")
  expect_error(silicate:::tri_engine("nope"))
})

test_that("engine TRI handles inlandwaters holes like earcut", {
  skip_if(length(engines()) == 0L, "no triangulation engine installed")
  ear <- TRI(inlandwaters[1:2, ], engine = "earcut")
  tri <- TRI(inlandwaters[1:2, ], engine = engines()[1])
  expect_equal(sum(tri_area(tri)), sum(abs(tri_area(ear))), tolerance = 1e-9)
})
