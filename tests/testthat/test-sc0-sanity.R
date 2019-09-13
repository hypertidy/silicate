context("test-sc0-sanity")
x <- SC0(minimal_mesh)


test_that("SC0 round trip suite works", {
  skip_on_cran() ## we get warnings
  expect_silent({
  SC(x)
  SC0(x)
  plot(SC(x))
  plot(SC0(x))
  ## these test have to go in anglr
  #anglr::DEL(SC(x))
  #plot(anglr::DEL(SC(x)))
  sc_vertex(x)
  sc_coord(x)
  sc_node(x)
  sc_edge(x)
  sc_segment(x)
  sc_start(x)
  sc_end(x)
  sc_object(x)
})
})


 test_that("errors when SC0 round trip unsupported", {
   expect_error(sc_arc(x))
   expect_error(sc_path(x))
   expect_error(TRI(x))
   expect_error(TRI0(x))
   expect_error(ARC(x))
   expect_error(ARC0(x))
   expect_error(PATH(x))
   expect_error(PATH0(x))
   ## test must go in anglr
   #expect_error(DEL(x))
})


test_that("concat SC0 works", {
  sc <- SC0(minimal_mesh)
  sc1 <- SC0(minimal_line)
  expect_s3_class(c(sc, sc1), "SC0")
})
