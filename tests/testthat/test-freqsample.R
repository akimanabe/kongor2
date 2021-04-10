context("freqsample")

test_that("dataset exists", {
  expect_is(freqsample1, "data.frame")
  expect_equal(freqsample1 %>% nrow(), 81)
  expect_equal(freqsample1 %>% ncol(), 2)
  expect_equal(
    freqsample1 %>% dplyr::pull(Freq) %>% sum(),
    51.885,
    tolerance = 0.001
  )
})
