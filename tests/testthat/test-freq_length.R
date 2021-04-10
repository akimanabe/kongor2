context("freq_length")

test_that("function exists", {
  expect_is(freq_length, "function")
})

test_that("function converts length vector into frequency tibble", {
  set.seed(1)
  sampledata <- generate_sample(1000, 4)
  foo <- freq_length(sampledata)

  expect_is(foo, "data.frame")
  expect_equal(sum(foo$Freq, na.rm = TRUE), 1000)

  expect_equal(freq_length(c(101, 111, 127, 135), binwidth = 10) %>%
                 dplyr::pull(Length),
               c(100, 110, 120, 130))
  expect_equal(freq_length(c(101, 111, 127, 135), binwidth = 10) %>%
                 dplyr::pull(Freq),
               c(1, 1, 1, 1))

  expect_equal(freq_length(c(101, 111, 127, 135), binwidth = 20) %>%
                 dplyr::pull(Length),
               c(100, 120))
  expect_equal(freq_length(c(101, 111, 127, 135), binwidth = 20) %>%
                 dplyr::pull(Freq),
               c(2,2))
})
