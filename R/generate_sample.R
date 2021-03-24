generate_sample <- function(n, ages, p = c(300, 0.2, 0)) {
  dat <- tibble::tibble(age = c(1:ages)) %>%
    dplyr::mutate(Lmean = p[1] * (1 - exp(-p[2] * (age - p[3])))) %>%
    dplyr::mutate(Number = 100 * exp(-M * age),
                  Number = round(Number)) %>%
    dplyr::mutate(Ratio = Number / sum(Number)) %>%
    dplyr::mutate(Length =
                    purrr::map2(.x = Lmean, .y = Ratio,
                                function(Lmean, Ratio){
                                  rnorm(n * Ratio,
                                        mean = Lmean,
                                        sd = Lmean * 0.15)}))

  Lengths <- dat$Length %>% unlist()

  return(Lengths)
}
