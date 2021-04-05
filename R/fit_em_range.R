#' fit_em for range of numbers of modes
#'
#' @param dat length data to be fitted
#' @param ages vector of numbers of modes
#'
#' @return tibble with nested mclust result
#' @export
#'
#' @examples
#' \dontrun{
#'sampledata <- generate_sample(1000, 5)
#' fit_em_range(sampledata, 4)
#' fit_em_range(sampledata, c(1:10))}
fit_em_range <- function(dat, ages) {
  suppressWarnings(require(mclust))
  tibble::tibble(Ages =  ages) %>%
    dplyr::mutate(num = dplyr::row_number()) %>%
    dplyr::mutate(Result = purrr::map(Ages, function(x) fit_em(dat, x))) %>%
    dplyr::mutate(BICe = purrr::map(num, function(x) purrr::pluck(., "Result", x, "BIC", 1))) %>%
    dplyr::mutate(BICv = purrr::map(num, function(x) purrr::pluck(., "Result", x, "BIC", 2))) %>%
    dplyr::mutate(BICe = unlist(BICe),
                  BICv = unlist(BICv)) %>%
    dplyr::mutate(Loglik = purrr::map(num, function(x) purrr::pluck(., "Result", x, "loglik", 1)),
                  Loglik = unlist(Loglik)) %>%
    dplyr::mutate(AIC = (2 * Ages) - (2 * Loglik)) %>%
    dplyr::mutate(Proportions = purrr::map(num, function(x) purrr::pluck(., "Result", x, "parameters", 1))) %>%
    dplyr::mutate(Means = purrr::map(num, function(x) purrr::pluck(., "Result", x, "parameters", 2))) %>%
    dplyr::mutate(SDs = purrr::map(num, function(x) purrr::pluck(., "Result", x, "parameters", 3, "sigmasq"))) %>%
    dplyr::select(Ages, AIC, BICe, BICv, Loglik, Proportions, Means, SDs, Result)
}
