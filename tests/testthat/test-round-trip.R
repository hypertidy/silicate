#context("round trip")
#skip("no round trip")  ## there's nothing to round trip ATM 2017-01-06

# test_that("round trip works", {
#   path <- PATH(mmesh) %>%
#     expect_s3_class("PATH")  %>%
#     expect_s3_class("sc") %>% PRIMITIVE() %>% expect_s3_class("PRIMITIVE") %>%
#     PATH()
#   path %>% sc_coord() %>% expect_s3_class("tbl_df")
#   path %>% sc_path() %>% expect_s3_class("tbl_df")
#   path %>% sc_object() %>% expect_s3_class("tbl_df")
#
#   prim <- PRIMITIVE(mmesh)
#   prim %>% sc_coord() %>% expect_s3_class("tbl_df")
#   prim %>% sc_path() %>% expect_s3_class("tbl_df")
#   prim %>% sc_object() %>% expect_s3_class("tbl_df")
#
#
# })
