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
