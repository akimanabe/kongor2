context("generate_sample")

test_that("function exists", {
  expect_is(generate_sample, "function")
})

test_that("function returns a numerical vector", {
  expect_is(generate_sample(100, 3), "numeric")
})

test_that("function returns a vector of length n", {
  num <- 100
  expect_equal(
    length(generate_sample(num, 3)), num)
})
