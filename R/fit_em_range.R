#' fit_em for range of numbers of modes
#'
#' @param dat length data to be fitted
#' @param Ages vector of numbers of modes
#'
#' @return tibble with nested mclust result
#' @export
#'
#' @examples
#' \dontrun{
#'sampledata <- generate_sample(1000, 5)
#' fit_em_range(sampledata, 4)
#' fit_em_range(sampledata, c(1:10))}
fit_em_range <- function(dat, Ages){
  suppressWarnings(require(mclust))
  tibble::tibble(Ages =  Ages) %>%
    dplyr::mutate(Result = purrr::map(Ages, function(x) fit_em(dat, x)))
}
