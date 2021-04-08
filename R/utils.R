#' Generate length frequency ratio
#'
#' @param dat tibble with column = c(Length, Freq)
#'
#' @return tibble with Freq_ratio
freq_ratio <- function(dat) {
  dat %>%
    dplyr::mutate(Freq_ratio = Freq / sum(Freq, na.rm = TRUE),
                  Freq_ratio = tidyr::replace_na(Freq_ratio, 0))
}
