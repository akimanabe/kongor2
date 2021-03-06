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
  res <- fit_em_range(sampledata, c(1:5))
  expect_equal(res %>%
                 colnames(),
               c("Ages", "AIC", "BICe", "BICv", "Loglik", "Proportions", "Means", "SDs", "Result"))

  expect_is(res %>%
              dplyr::pull(AIC),
            "numeric")
})
