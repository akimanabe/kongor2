#' Generate sample length data
#'
#' @param n Total number of sample
#' @param ages Number of ages, the maximum age
#' @param p Parameters of von Bertalanffy growth function
#' Linf: Asymptotic body length
#' K: growth coefficient
#' t0: Hypothetical age at size = 0
#'
#' @return Vector of generated length with sample size of n
#' @export
#'
#' @examples
#' \dontrun{
#' foo <-generate_sample(n = 1000, ages = 7)
#' hist(foo)}
generate_sample <- function(n, ages, p = c(300, 0.2, 0)) {
  mortality <- 0.4 # sample natural mortality
  dat <- tibble::tibble(age = c(1:ages)) %>%
    dplyr::mutate(l_mean = p[1] * (1 - exp(-p[2] * (age - p[3])))) %>%
    dplyr::mutate(Number = 100 * exp(-mortality * age),
                  Number = round(Number)) %>%
    dplyr::mutate(ratio = Number / sum(Number),
                  ratio = ratio * 100,
                  ratio = ceiling(ratio) / 100) %>%
    dplyr::mutate(Length =
                    purrr::map2(.x = l_mean, .y = ratio,
                                function(l_mean, ratio) {
                                  rnorm(n * ratio,
                                        mean = l_mean,
                                        sd = l_mean * 0.15)}))

  lengths <- dat$Length %>%
    unlist() %>%
    sample(n)

  return(lengths)
}
