context("freq_length")

test_that("function exists", {
  expect_is(freq_length, "function")
})

test_that("function converts length vector into frequency tibble", {
  sample_number <- 1000
  foo <- freq_length(generate_sample(sample_number, 4))

  expect_is(foo, "data.frame")
  expect_equal(sum(foo$Freq, na.rm = TRUE), sample_number)
  })
