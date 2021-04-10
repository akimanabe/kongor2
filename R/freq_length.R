#' Create length frequency
#'
#' @param dat length vector
#' @param binwidth width of length bin, 10 as standard
#'
#' @return tibble of length bin (1cm) and its frequency
#' @export
#'
#' @examples
#' \dontrun{
#' freq_length(
#' generate_sample(1000, 5), binwidth = 10)}
freq_length <- function(dat, binwidth = 10) {
  dat %>%
    tibble::tibble(Length = dat) %>%
    dplyr::mutate(Length = floor(Length / binwidth) * binwidth) %>%
    dplyr::group_by(Length) %>%
    dplyr::summarize(Freq = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(.,
                     tibble::tibble(
                       Length = seq(
                         min(floor(dat / binwidth) * binwidth),
                         max(floor(dat / binwidth) * binwidth),
                         by = binwidth
                       )
                     )) %>%
    dplyr::arrange(Length)
}
