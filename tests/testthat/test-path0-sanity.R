context("test-path0-sanity")
x <- PATH0(minimal_mesh)


test_that("PATH0 round trip suite works", {
  skip_on_cran()  ## we get warnings here
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
  sc_object(x)
  sc_path(x)
  PATH(x)
  PATH0(x)
 sc_start(x)
 sc_end(x)
  })

  expect_warning({
    ARC(x)
    sc_arc(x)
  }
  )
  expect_s3_class(TRI0(x), "TRI0")
  expect_s3_class(  TRI(x), "TRI")
})


 test_that("errors when PATH0 round trip unsupported", {

   expect_error(ARC0(x))

##   expect_error(sc_start(x))
##   expect_error(sc_end(x))
   ## test must go in anglr
   #expect_error(DEL(x))
})

