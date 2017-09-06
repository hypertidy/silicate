# context("primitives")
# 
# #
# # test_that("primitives 0D", {
# #   error("no tests!")
# # })
# test_that("primitives 1D", {
#   # inlandwaters[5, ] 
#   minimal_mesh %>% PRIMITIVE() %>% expect_s3_class("PRIMITIVE") %>% 
#     ## test round-trip back to sf
#     sf() %>% expect_s3_class("sf") %>% PRIMITIVE() %>% sf() %>% expect_s3_class("sf")
#   
#   
# })
