context("estim_resids")

test_that("function exists", {
  expect_is(estim_resids, "function")
})

test_that("function returns residual number properly", {
  set.seed(1)
  foodata <-
    generate_sample(n = 100, ages = 3) %>%
    freq_length() %>%
    freq_ratio()

  expect_is(
  estim_resids(dat = foodata,
               means = c(50, 80, 120),
               sds = c(10, 20, 30)),
  "numeric")

  expect_equal(
    estim_resids(dat = foodata,
                 means = c(50, 80, 120),
                 sds = c(10, 20, 30)) %>%
      round(., digits = 3),
    0.107
  )

})

context("estim_optim")

test_that("function exists", {
  expect_is(estim_optim, "function")
})

test_that("function estimates parameters", {
  set.seed(1)
  foodata <-
    generate_sample(n = 100, ages = 3) %>%
    freq_length() %>%
    freq_ratio()

  res <- estim_optim(foodata,
                     means = c(50, 80, 120),
                     sds = c(10, 20, 30))
  expect_is(res, "data.frame")

  expect_equal(colnames(res), c("parameter", "value"))

})
