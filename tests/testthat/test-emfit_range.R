context("emfit_range")

test_that("function exists", {
  expect_is(emfit_range, "function")
})

test_that("function estimates parameters via em", {
  sampledata <- generate_sample(1000, 5)
  expect_is(emfit_range(sampledata, 5), "Mclust")
})

test_that("function fits different numbers of modes", {
  sampledata <- generate_sample(1000, 5)
  expect_is(emfit_range(sampledata, c(1:10)), "list")
})
