#' Fit length data using EM algorithm
#'
#' @param dat length vector
#' @param age number of modes of gaussian to be fitted
#'
#' @return Mclust result
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- generate_sample(100, 3)
#' fit_em(foo, 3)}
fit_em <- function(dat, age){
  mclust::Mclust(dat, G = age)
}

#' Apply fit_em with various mode number to the data
#'
#' @param dat length vector
#' @param ages vector of number of modes to be applied
#'
#' @return listed Mclust result
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- generate_sample(100, 3)
#' fit_range(foo, ages = c(2:5))}
fit_range <- function(dat, ages = c(2:4)) {
purrr::map(ages, function(x) {fit_em(dat, ages)})
}
