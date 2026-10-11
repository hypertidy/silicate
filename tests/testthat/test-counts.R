context("test-counts")

## Regression guard for #144: string literals in data-masking verbs silently
## broke grouping/joins (e.g. SC() returned a single edge). Counts are the
## silicate 0.7.1 CRAN values. inlandwaters is bundled, so no sf namespace
## is needed.

test_that("SC(inlandwaters) counts match the 0.7.1 benchmark", {
  sc_iw <- SC(inlandwaters)
  expect_equal(nrow(sc_iw$vertex), 30835L)
  expect_equal(nrow(sc_iw$edge), 30843L)
})

test_that("SC() on a TRI has a de-duplicated edge table", {
  e <- SC(TRI(minimal_mesh))$edge
  expect_equal(nrow(e), dplyr::n_distinct(e$edge_))
})
