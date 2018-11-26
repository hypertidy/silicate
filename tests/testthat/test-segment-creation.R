context("segment-creation")
mm <- PATH(minimal_mesh)
ps <- PATH(polymesh)
segsmm <- sc_segment_base(mm$path_link_vertex)
segsps <- sc_segment_base(ps$path_link_vertex)
minsegs <- c(7L, 1L, 2L, 3L, 4L, 5L, 6L, 12L, 8L, 9L, 10L, 11L, 6L, 5L,
             14L, 15L)
polysegs <- c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 1L, 56L, 50L, 49L, 48L, 47L, 46L, 45L, 44L, 43L, 65L, 66L, 67L, 68L, 55L, 70L, 71L, 72L, 52L, 51L, 50L, 56L, 55L, 68L, 67L, 66L, 81L, 82L, 69L, 84L, 85L, 86L, 66L, 65L, 43L, 42L, 41L, 40L, 39L, 94L, 83L, 96L, 97L, 98L, 1L, 54L, 53L, 52L, 72L, 71L, 70L, 106L, 95L, 108L, 109L, 110L, 84L, 83L, 94L, 39L, 38L, 37L, 36L, 35L, 119L, 120L, 107L, 96L, 123L, 124L, 125L, 2L, 1L, 98L, 97L, 130L, 131L, 108L, 107L, 120L, 119L, 35L, 34L, 138L, 129L, 140L, 141L, 142L, 143L, 70L, 69L, 82L, 81L, 66L, 86L, 85L, 84L, 152L, 139L, 154L, 155L, 156L, 96L, 95L, 106L, 70L, 143L, 142L, 141L, 140L, 165L, 166L, 153L, 168L, 169L, 170L, 140L, 139L, 152L, 84L, 110L, 109L, 108L, 178L, 167L, 166L, 181L, 154L, 153L, 156L, 155L, 154L, 187L, 188L, 189L, 123L, 96L, 192L, 168L, 167L, 178L, 108L, 131L, 130L, 199L, 200L, 191L, 202L, 203L, 204L, 166L, 165L, 140L, 170L, 169L, 168L, 201L, 181L, 166L, 204L, 203L, 202L, 217L, 218L, 219L, 220L, 221L, 187L, 154L, 202L, 201L, 168L, 192L, 191L, 200L, 230L, 231L, 232L, 223L, 234L, 235L, 218L, 217L, 202L, 223L, 232L, 233L)

test_that("segment creation works", {
  expect_identical(match(segsmm$.vx0, segsmm$.vx1) , minsegs)
  expect_identical(match(segsps$.vx1, segsps$.vx0), polysegs)
})
