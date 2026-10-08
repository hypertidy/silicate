skip_if_not_installed("RNetCDF")
skip_if_not_installed("wk")
skip_if_not_installed("wkpool")
skip_if_not_installed("meshcore")

# A small mixed mesh: two triangles and a quad, the triangles clockwise
mixed <- function() {
  node <- data.frame(x = c(0, 1, 2, 0, 1, 2), y = c(0, 0, 0, 1, 1, 1))
  m <- rbind(c(1, 2, 5, 4), c(2, 3, 5, NA), c(3, 5, 6, NA))
  UGRID(node, m)
}

test_that("a mixed mesh gets edges, sides and neighbours", {
  x <- ugrid_orient(mixed())
  expect_equal(nrow(x$face), 3)
  expect_equal(x$face$n, c(4L, 3L, 3L))
  expect_equal(nrow(x$edge), 8)
  expect_equal(nrow(boundary_edges(x)), 6)
  nb <- meshcore::neighbours(x)
  expect_equal(nrow(nb), 4)
  expect_setequal(paste(nb$.cell, nb$.neighbour), c("0 1", "1 0", "1 2", "2 1"))
})

test_that("silicate verbs work on UGRID", {
  x <- ugrid_orient(mixed())
  expect_named(sc_vertex(x), c("x_", "y_", "vertex_"))
  expect_equal(nrow(sc_edge(x)), 8)
  expect_equal(nrow(sc_object(x)), 3)
  expect_equal(nrow(sc_coord(x)), 10)
})

test_that("validation reports clockwise faces and orient fixes them", {
  x <- mixed()
  v <- zoo_validate(x)
  expect_false(v$ok[v$check == "faces counter-clockwise"])
  o <- ugrid_orient(x)
  expect_equal(attr(o, "flipped"), 1L)
  expect_true(all(zoo_validate(o)$ok))
  # before orienting, the flipped face puts two faces on one side of an edge
  expect_false(v$ok[v$check == "manifold (one face per edge side)"])
})

test_that("a grid descriptor becomes quads in GDAL cell order", {
  g <- as_UGRID(grid_descriptor(c(0, 4, 0, 3), c(4, 3)))
  expect_equal(c(nrow(g$node), nrow(g$face), nrow(g$edge)), c(20, 12, 31))
  expect_true(all(zoo_validate(g)$ok))
  # cell 0 is the top-left cell
  cl <- meshcore::cells(g)
  expect_equal(unlist(cl[1, c("x", "y")]), c(x = 0.5, y = 2.5))
  # drawable conformance case from the mesh spec: 20 vertices, 24 triangles
  d <- as_drawable(g)
  expect_equal(nrow(d$vertices), 20)
  expect_equal(length(d$indices) / 3, 24)
  expect_equal(range(d$indices), c(0, 19))
})

test_that("polygons through wkpool give faces and count lost holes", {
  p <- wk::wkt(c("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0), (0.5 0.5, 0.5 1, 1 1, 0.5 0.5))",
                 "POLYGON ((2 0, 3 0, 3 2, 2 2, 2 0))"))
  u <- as_UGRID(p)
  expect_equal(nrow(u$face), 2)
  expect_equal(u$meta$lost[["holes"]], 1)
  expect_equal(nrow(meshcore::neighbours(u)), 2)
  expect_true(all(zoo_validate(u)$ok))
  # and back: the same faces as polygons
  expect_equal(nrow(as_UGRID(ugrid_polygons(u))$face), 2)
})

test_that("lines give a 1D mesh", {
  l <- as_UGRID(wk::wkt(c("LINESTRING (0 0, 1 0, 1 1)", "LINESTRING (1 1, 2 2)")))
  expect_equal(l$meta$topology_dimension, 1L)
  expect_equal(c(nrow(l$node), nrow(l$edge), nrow(l$face)), c(4, 3, 0))
})

test_that("netCDF round trip keeps topology and arrays", {
  x <- ugrid_orient(mixed())
  f <- tempfile(fileext = ".nc")
  val <- structure(c(10, 20, 30), location = "face")
  write_ugrid(x, f, arrays = list(depth = val))
  y <- read_ugrid(f)
  expect_identical(y$face_node, x$face_node)
  expect_equal(y$node, x$node)
  expect_identical(y$edge, x$edge)
  expect_equal(y$meta$edge_source, "stored")
  a <- ugrid_arrays(f, y)
  expect_equal(a$variable, "depth")
  ka <- ugrid_read_array(f, "depth", y)
  j <- meshcore::join_array(y, ka)
  expect_equal(j$value, c(10, 20, 30))
  expect_equal(j$.cell, 0:2)
})

test_that("fan triangulation keeps the face key", {
  t <- as_tri(ugrid_orient(mixed()))
  expect_equal(nrow(t$triangles), 4)
  expect_equal(t$triangles$face, c(1L, 1L, 2L, 3L))
  expect_equal(attr(t, "fan_ok"), 4L)
  back <- as_UGRID(t)
  expect_equal(nrow(back$face), 4)
})

test_that("the zoo matrix lists routes", {
  m <- zoo_matrix()
  expect_equal(m["UGRID", "TRI"], "lossless")
  expect_equal(m["polygons (wk)", "UGRID"], "lossy")
  expect_true(all(c("UGRID", "GRID", "CELL") %in% zoo_models()$model))
})

# Real files, when present (uxarray and MDAL test data; not shipped)
# UGRID_TESTDATA: a folder with sparse clones of UXARRAY/uxarray
# (test/meshfiles) and lutraconsulting/MDAL (tests/data/ugrid)
real <- function(...) {
  root <- Sys.getenv("UGRID_TESTDATA")
  if (!nzchar(root)) skip("UGRID_TESTDATA not set")
  f <- file.path(root, ...)
  if (!file.exists(f)) skip("test mesh not available")
  f
}

test_that("D-Flow FM: mixed faces, stored edges, 1-based, time arrays", {
  f <- real("MDAL/tests/data/ugrid/D-Flow1.1/simplebox_hex7_map.nc")
  x <- read_ugrid(f)
  expect_equal(c(nrow(x$node), nrow(x$edge), nrow(x$face)), c(720, 1529, 810))
  expect_equal(x$meta$start_index, 1L)
  expect_true(all(zoo_validate(x)$ok))
  s1 <- ugrid_read_array(f, "mesh2d_s1", x)
  expect_equal(unname(s1$dims), c(810, 13))
  expect_equal(nrow(meshcore::join_array(x, s1)), 810 * 13)
  u1 <- ugrid_read_array(f, "mesh2d_u1", x, time = 13)
  expect_equal(nrow(meshcore::join_array(x, u1)), 1529)
  expect_equal(nrow(meshcore::neighbours(x)) / 2,
               nrow(wkpool::find_neighbours(meshcore::as_wkpool(x))))
})

test_that("FESOM: transposed connectivity, 1-based, clockwise faces", {
  f <- real("uxarray/test/meshfiles/ugrid/fesom/fesom.mesh.diag.nc")
  x <- read_ugrid(f)
  expect_equal(c(nrow(x$node), nrow(x$face)), c(3140, 5839))
  expect_equal(x$meta$start_index, 1L)
  v <- zoo_validate(x)
  expect_false(v$ok[v$check == "faces counter-clockwise"])
})

test_that("cubed sphere: data file names the face dimension ncol", {
  x <- read_ugrid(real("uxarray/test/meshfiles/ugrid/outCSne30/outCSne30.ug"))
  f <- real("uxarray/test/meshfiles/ugrid/outCSne30/outCSne30_vortex.nc")
  expect_equal(nrow(ugrid_arrays(f, x)), 0)
  psi <- ugrid_read_array(f, "psi", x, dim_map = c(ncol = "face"))
  expect_equal(nrow(meshcore::join_array(x, psi, dim_map = c(ncol = "face"))), 5400)
})

test_that("meshcore GRID and HEALPix CELL convert, and a raster array survives netCDF", {
  skip_if_not_installed("meshcore")
  h <- meshcore::CELL(grid_name = "healpix", level = 2)
  uh <- as_UGRID(h)
  v <- zoo_validate(uh)
  expect_equal(v$value[v$check == "Euler characteristic V - E + F"], "2")
  expect_equal(nrow(meshcore::neighbours(uh)), nrow(meshcore::neighbours(h)))
  f <- system.file("extdata/field.tif", package = "meshcore")
  skip_if(!nzchar(f))
  g <- meshcore::GRID(f)
  a <- meshcore::read_array(f)
  ug <- as_UGRID(g)
  nc <- tempfile(fileext = ".nc")
  write_ugrid(ug, nc, arrays = list(field = structure(as.vector(a$values), location = "face")))
  back <- read_ugrid(nc)
  j <- meshcore::join_array(back, ugrid_read_array(nc, "field", back))
  expect_equal(j$value, meshcore::join_array(g, a)$value)
  expect_equal(j$x, meshcore::cells(g)$x)
})

test_that("stored edges drive edges(), face_edge(), counts and edge joins", {
  x <- ugrid_orient(mixed())
  e <- meshcore::edges(x)
  expect_equal(e$.edge, 0:7)
  expect_equal(nrow(meshcore::face_edge(x)), 10)
  expect_equal(unname(meshcore::mesh_counts(x)), c(3, 6, 8))
  l <- as_UGRID(wk::wkt("LINESTRING (0 0, 1 0, 1 1)"))
  expect_equal(length(meshcore::as_wkpool(l)), 2)
  f <- real("MDAL/tests/data/ugrid/D-Flow1.1/simplebox_hex7_map.nc")
  d <- read_ugrid(f)
  j <- meshcore::join_array(d, ugrid_read_array(f, "mesh2d_u1", d, time = 13))
  expect_equal(j[c(".edge", ".vx0", ".vx1")], meshcore::edges(d))
})
