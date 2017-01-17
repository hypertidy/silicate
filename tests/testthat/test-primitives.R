context("primitives")

#
# test_that("primitives 0D", {
#   error("no tests!")
# })
test_that("primitives 1D", {
   inlandwaters[5, ] %>% PRIMITIVE() %>% expect_s3_class("PRIMITIVE") %>% 
   ## test round-trip back to sf
   st_as_sf() %>% expect_s3_class("sf")
})
# 
# test_that("primitives 2D", {
#   error("no tests!")
# })
# test_that("primitives 3D", {
#   error("no tests!")
# })
