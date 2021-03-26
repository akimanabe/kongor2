context("fit_em")
library(mclust)
test_that("fit_em works", {
  expect_is(fit_em, "function")
  expect_is(fit_em(generate_sample(100, 3), 3), "Mclust")
})

context("fit_range")
library(mclust)
test_that("fit_range works", {
  expect_is(fit_range, "function")
  expect_is(fit_range(generate_sample(100, 3), ages = c(2:4)),
            "list")
})
