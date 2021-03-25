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
  M <- 0.4 # sample natural mortality
  dat <- tibble::tibble(age = c(1:ages)) %>%
    dplyr::mutate(Lmean = p[1] * (1 - exp(-p[2] * (age - p[3])))) %>%
    dplyr::mutate(Number = 100 * exp(-M * age),
                  Number = round(Number)) %>%
    dplyr::mutate(Ratio = Number / sum(Number),
                  Ratio = Ratio * 100,
                  Ratio = ceiling(Ratio) / 100) %>%
    dplyr::mutate(Length =
                    purrr::map2(.x = Lmean, .y = Ratio,
                                function(Lmean, Ratio){
                                  rnorm(n * Ratio,
                                        mean = Lmean,
                                        sd = Lmean * 0.15)}))

  Lengths <- dat$Length %>%
    unlist() %>%
    sample(n)

  return(Lengths)
}
