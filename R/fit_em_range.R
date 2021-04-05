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
    dplyr::mutate(Result = purrr::map(Ages, function(x) fit_em(dat, x))) %>%
    dplyr::mutate(BIC = purrr::pluck(., "Result", 1, "BIC"),
                  BIC_E = BIC[,"E"], #nolint
                  BIC_V = BIC[,"V"]) #nolint
}
