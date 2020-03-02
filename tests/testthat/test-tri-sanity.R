context("test-TRI-sanity")
x <- TRI(minimal_mesh)
skip_on_cran()
test_that("TRI round trip suite works", {
  expect_silent({
  SC(x)
  SC0(x)
  # must go in anglr
  #anglr::DEL(SC(x))  ## have to SC it first
  #plot(anglr::DEL(SC(x)))

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

  TRI(x)
  TRI0(x)
})
})


 test_that("errors when SC0 round trip unsupported", {
   expect_error(sc_arc(x))
   expect_error(sc_path(x))
   ## put in anglr
#   expect_error(anglr::DEL(x))

   expect_error(ARC(x))
   expect_error(ARC0(x))
   expect_error(PATH(x))
   expect_error(PATH0(x))
})
