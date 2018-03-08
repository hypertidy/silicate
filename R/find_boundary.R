sc_identify_object_boundary <- function(x, ...) {
  ## x is a TRI
  ## this code is the beginning of SC.TRI
  segment <- purrr::map_df(purrr::transpose(x$triangle[c(".vertex0", ".vertex1", ".vertex2")]), 
                           ~to_tibble(tri_to_seg(unlist(.x))), .id = "triangle_")
  segment$triangle_ <- x$triangle$triangle_[as.integer(segment$triangle_)]
  #segment$segment_ <- silicate::sc_uid(nrow(segment))
  edges <- as.integer(factor(apply(cbind(segment$.vertex0, segment$.vertex1), 1, 
                                   function(x) paste(sort(x), collapse = "-"))))
  segment$edge_ <- sc_uid(length(unique(edges)))[edges]
  ## segment is now a map of triangle_ to edge_  
  ## and so we can build a map of object, triangle, edge
  ## where SC.TRI would not keep triangle in the distinct
  object_link_edge <- dplyr::inner_join(x$object_link_triangle, 
                                        segment[c("edge_", "triangle_")]) %>% 
    dplyr::distinct(object_, edge_, triangle_)
  
  ## so within object, what edges don't have two triangles (assuming holes aren't fille by triangles)
  boundary_edges <- purrr::map(split(object_link_edge, object_link_edge$object_), 
                               ~.x %>% dplyr::group_by(edge_) %>% 
    dplyr::filter(n() < 2) %>% dplyr::ungroup() %>% dplyr::distinct(edge_)) %>% 
    dplyr::bind_rows()

  
  boundary_edges %>% 
    dplyr::inner_join(segment[c(".vertex0", ".vertex1", "edge_")]) %>% 
    tidyr::gather(node, vertex_, -edge_) %>% dplyr::inner_join(x$vertex)
  
}
