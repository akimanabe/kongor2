#' Create length frequency
#'
#' @param dat length vector
#'
#' @return tibble of length bin (1cm) and its frequency
#' @export
#'
#' @examples
#' \dontrun{
#' freq_length(
#' generate_sample(1000, 5))}
freq_length <- function(dat) {
  dat %>%
    tibble::tibble(Length = dat) %>%
    dplyr::mutate(Length = floor(Length)) %>%
    dplyr::group_by(Length) %>%
    dplyr::summarize(Freq = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(.,
                     tibble::tibble(
                       Length = seq(
                         min(floor(dat)),
                         max(floor(dat))
                       )
                     )) %>%
    dplyr::arrange(Length)
}
