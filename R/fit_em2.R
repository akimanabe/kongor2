fit_em2 <- function(dat, ages, vb_param) {
  mu <- vb_param[1] * (1 - exp( -vb_param[2] * (ages - vb_param[3])))
  mixtools::normalmixEM(dat, mean.constr = mu, k = length(ages))
}
