context("test-SC-sanity")
x <- SC(minimal_mesh)

test_that("SC round trip suite works", {
  skip_on_cran()  ## we get warnings
  expect_silent({
  SC(x)
  SC0(x)
  ## must go in anglr
  #anglr::DEL(x)
  #plot(anglr::DEL(x))

  plot(SC(x))
  plot(SC0(x))
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
})
