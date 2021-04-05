context("fit_em_range")

test_that("function exists", {
  expect_is(fit_em_range, "function")
})

test_that("function estimates parameters via em", {
  sampledata <- generate_sample(1000, 5)
  expect_is(fit_em_range(sampledata, 5), "data.frame")
})

test_that("function fits different numbers of modes", {
  sampledata <- generate_sample(1000, 5)
  expect_is(fit_em_range(sampledata, c(1:10)), "data.frame")
})

test_that("result has a BIC column", {
  sampledata <- generate_sample(1000, 5)
  expect_equal(fit_em_range(sampledata, c(1, 2, 3, 4, 5)) %>%
                 dplyr::select(BIC) %>%
                 colnames(),
               "BIC")
  expect_is(fit_em_range(sampledata, c(1, 2, 3, 4, 5)) %>%
                 dplyr::pull(BIC),
               "numeric")
})
