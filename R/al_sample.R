#' Generate Age-Length sample with specified age and sd
#'
#' @param N total number of sample to be produced
#' @param ages vector of ages
#' @param ps von bertalanffy parameters
#' @param lambda ratio of each gaussian. vector with length = that of ages
#' @param strength_sd greater the number, smaller the sd
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' al_sample(100, c(1, 2, 3), ps = c(500, 0.5, -0.3), lambda = c(1, 2, 3))
#' }
al_sample <- function(N, ages, ps, lambda, strength_sd = 10) {

  means <- fishgr::vb(age = ages, ps = ps, setorigin = FALSE) %>% round()
  sds <- abs(means / 10)

  tibble::tibble(ages = ages, means = means, sds = sds, lambda) %>%
    dplyr::mutate(
      Length = purrr::map2(means, sds,
                           function(x, y) rnorm(N, mean = x, sd = y)),
      lambda = lambda / sum(lambda),
      lambda_N = ceiling(N * lambda),
      Length_N = purrr::map2(Length, lambda_N,
                             function(x, y) sample(x, y, replace = FALSE))) %>%
    dplyr::select(ages, means, sds, lambda, Length_N) %>%
    tidyr::unnest(cols = c(Length_N)) %>%
    dplyr::mutate(ages = factor(ages)) %>%
    dplyr::rename(Length = Length_N) %>%
    dplyr::sample_n(., N)

}
