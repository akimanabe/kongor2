fit_em <- function(dat, age){
  mclust::Mclust(dat, G = age)
}

fit_range <- function(dat, ages = c(2:4)) {
purrr::map(ages, function(x) {fit_em(dat, ages)})

}
