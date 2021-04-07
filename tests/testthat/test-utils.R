context("freq_ratio")

test_that("function exists", {
  expect_is(freq_ratio, "function")
})

test_that("function converts Freq into ratio", {
  foo <-
    generate_sample(100, 3) %>%
    freq_length() %>%
    freq_ratio()

  expect_equal(ncol(foo), 3)
  expect_equal(colnames(foo),
               c("Length", "Freq", "Freq_ratio"))
  expect_true(
    !is.na(sum(foo$Freq_ratio, na.rm = FALSE))
  )
})

context("length_bin")

test_that("function exists", {
  expect_is(length_bin, "function")
})

test_that("function converts length into bin", {
  foo <- c(111, 128, 305)
  expect_equal(
    foo %>%
      length_bin() %>%
      dplyr::pull(Length),
    c(110, 120, 300))
})
