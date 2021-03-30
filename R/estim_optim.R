#' Calculate sum of residual for specified means and sds
#'
#' @param dat data with columns `c("Length", "Freq", "Freq_ratio")`
#' @param means means of each modes. number of means represents number of modes
#' @param sds sd of each distribution.
#' vector length must be the same as that of means
#'
#' @return sum of residuals
#' @export
#'
#' @examples
#' \dontrun{
#' foodata <-
#' generate_sample(n = 100, ages = 3) %>%
#' freq_length() %>%
#' freq_ratio()
#'
#' estim_resids(dat = foodata, means = c(50, 80, 120), sds =c(10, 20, 30))}
estim_resids <-
  function(dat, means, sds) {
    assertthat::assert_that(length(means) == length(sds),
                            msg = "length of means and sds must be the same")
    dat %>%
      dplyr::slice(rep(dplyr::row_number(), length(means))) %>%
      dplyr::bind_cols(.,
                       tibble::tibble(mean = rep(means, each = nrow(dat)),
                                      sd   = rep(sds, each = nrow(dat)))
      ) %>%
      dplyr::mutate(dnorms = dnorm(Length, mean, sd)) %>%
      dplyr::group_by(Length, Freq, Freq_ratio) %>%
      dplyr::summarise(dnorm_sum = sum(dnorms)) %>%
      dplyr::mutate(dnorm_estim = dnorm_sum / length(means)) %>%
      dplyr::mutate(resids = (Freq_ratio - dnorm_estim)^2) %>%
      dplyr::pull(resids) %>%
      sum(., na.rm = TRUE)
  }

#' Estimate means and sds with multiple gaussian distribution to data
#'
#' @param dat data with columns `c("Length", "Freq", "Freq_ratio")`
#' @param means means of each modes. number of means represents number of modes
#' @param sds sd of each distribution.
#' vector length must be the same as that of means
#'
#' @return tibble of parameters estimated
#' @export
#'
#' @examples
#' \dontrun{
#' foodata <-
#' generate_sample(n = 100, ages = 3) %>%
#' freq_length() %>%
#' freq_ratio()
#'
#' estim_optim(foodata, means = c(50, 80, 120), sds = c(10, 20, 30))
#' }
estim_optim <-
  function(dat, means, sds) {
    ps <- c(means, sds)
    pnum <- length(ps)
    pnumhalf <- length(ps) / 2
    resid2optim <-
      function(ps) {
        estim_resids(dat = dat,
                     means = ps[1:(length(ps) / 2)],
                     sds = ps[(length(ps) / 2 + 1): (length(ps))])
      }
    optim(ps, resid2optim, method = "L-BFGS-B", lower = 0) %>%
      broom::tidy()
  }
