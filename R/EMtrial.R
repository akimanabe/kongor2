# ggplot2::ggplot(baz) +
#   ggplot2::geom_col(ggplot2::aes(x = Length, y = Freq_ratio))
#
# # log_likelihood <- function(x, mu, sigma, pi) {
# #   sum(log(pi[1] * dnorm(x, mu[1], sqrt(sigma[1])) +
# #             pi[2] * dnorm(x, mu[2], sqrt(sigma[2]))))
# # }

log_likelihood.7 <- function(x, mu, sigma, pi) {
  sum(
    log(
      pi[1] * dnorm(x, mu[1], sqrt(sigma[1])),
      pi[2] * dnorm(x, mu[2], sqrt(sigma[2])),
      pi[3] * dnorm(x, mu[3], sqrt(sigma[3])),
      pi[4] * dnorm(x, mu[4], sqrt(sigma[4])),
      pi[5] * dnorm(x, mu[5], sqrt(sigma[5])),
      pi[6] * dnorm(x, mu[6], sqrt(sigma[6])),
      pi[7] * dnorm(x, mu[7], sqrt(sigma[7]))
      )
    )
}
mu <- round(fishgr::ext_vb(c(1:7), ps = c(615.9, 0.342, 0.17, 1.61, -0.88)) / 10, 1)

sigma <- rep(2, 7)
pis <- c(3, 3, 2, 2, 1, 1, 1)
pi <- pis / sum(pis)
# gamma_0 <- c()
# gamma_1 <- c()
# n_k <- c()
#
# log_likelihood_history <- c()
#
# for(step in 1:1000) {
#   old_log_likelihood <- log_likelihood(x, mu, sigma, pi)
#   log_likelihood_history <- c(log_likelihood_history, old_log_likelihood)
#
#   # E-step
#   # gamma_0はクラス0の混合係数、gamma_1はクラス1の混合係数
#   gamma_1 <- pi[1] * dnorm(x, mu[1], sqrt(sigma[1])) /
#     (pi[1] * dnorm(x, mu[1], sqrt(sigma[1])) + pi[2] * dnorm(x, mu[2], sqrt(sigma[2])))
#   gamma_2 <- 1 - gamma_1
#
#   # M-step
#   n_k[1] <- sum(gamma_1)
#   n_k[2] <- sum(gamma_2)
#   mu[1] <-  sum(gamma_1 * x) / n_k[1]
#   mu[2] <-  sum(gamma_2 * x) / n_k[2]
#   sigma[1] <- sum(gamma_1 * (x - mu[1])^2) / n_k[1]
#   sigma[2] <- sum(gamma_2 * (x - mu[2])^2) / n_k[2]
#   pi[1] <- n_k[1] / N
#   pi[2] <- 1 - pi[1]
#   if(abs(log_likelihood(x, mu, sigma, pi) - old_log_likelihood) < 0.001){
#     break
#   }
# }
