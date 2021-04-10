context("al_sample")

test_that("function works", {
  set.seed(1)
  foo <- al_sample(100, c(1, 2), ps = c(100, 0.6, 0), lambda = c(1, 2))
  expect_is(al_sample, "function")
  expect_is(foo, "data.frame")
  expect_equal(nrow(foo), 100)
  expect_equivalent(kmeans(foo, 2)$centers[,5], c(45, 70), tolerance = 1)
})
