#' Generate Age-Length sample
#'
#' @param N total number of sample to be generated
#' @param ages vector of ages which distribs are based on
#' @param ps von bert param vector
#' @param lambda vector of proportion
#' @param strength_sd default = 10, greater the number smaller the sd
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' al_sample(100, ages = c(1, 2), ps = c(300, 0.6, 0), lambda = c(3, 1))}
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
