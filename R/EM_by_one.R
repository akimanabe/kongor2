foomeans <- fishgr::vb(c(0, 1, 2), ps = c(480, 0.6, -0.5)) %>% round(-1) #means
foolamda <- c(1, 0.6, 0.2) / sum(c(1, 0.6, 0.2))
foosd <- c(foomeans/5)
set.seed(1)
foo1 <- rnorm(100 * foolamda[1], mean = foomeans[1], sd = foosd[1])
foo2 <- rnorm(100 * foolamda[2], mean = foomeans[2], sd = foosd[2])
foo3 <- rnorm(100 * foolamda[3], mean = foomeans[3], sd = foosd[3])

foo <- c(foo1, foo2, foo3)
hist(foo, breaks = seq(0, ceiling(max(foo/100))*100, by = 10))

fit_em2 <- function(dat, ages, vb_param) {
  mu <- vb_param[1] * (1 - exp( -vb_param[2] * (ages - vb_param[3])))
  mixtools::normalmixEM(dat, mean.constr = mu, k = length(ages))
}

res <- fit_em2(foo, ages = c(0, 1, 2), vb_param = c(480, 0.6, -0.5))

hist(foo, xlim = c(0, 500), breaks = seq(0, ceiling(max(foo/100))*100, by = 10))
par(new = TRUE)
curve(dnorm(x,res$mu[1],res$sigma[1]) * res$lambda[1], 0, 500, col = "red");par(new = TRUE)
curve(dnorm(x,res$mu[2],res$sigma[2]) * res$lambda[2], 0, 500, add = TRUE, col = "green")
curve(dnorm(x,res$mu[3],res$sigma[3]) * res$lambda[3], 0, 500, add = TRUE, col = "blue")
# const as vb
# $loglik
# [1] -558.4056

res2 <- fit_em2(foo, ages = c(0, 1, 2), vb_param = c(480, 0.6, -0.5))

####

# MY-EM_ALGORITHM

# Initial parameter sets
ps <-
  tibble::tibble(age = c(1, 2, 3),
                 means = c(100, 300, 400),
                 sds = c(10, 30, 40),
                 prop = c(5, 3, 2)) %>%
  dplyr::mutate(prop = prop/sum(prop))

# Data to be fitted
dat <- foo

# Oneliner
ps %>%
  dplyr::mutate(x = purrr::map2(prop, means, function(x, y) x * dnorm(dat, mean = y, sd = sds))) %>%
  dplyr::mutate(sumx = purrr::map(x, function(x) sum(x))) %>%
  tidyr::unnest(sumx) %>%
  dplyr::mutate(probs = purrr::map2(x, sumx, function(x, y) x / y)) %>% #calculate probability
  dplyr::mutate(means = purrr::map(probs, function(x) sum(x * dat) / sum(x))) %>%
  tidyr::unnest(means) %>%
  dplyr::mutate(newprop = purrr::map(probs, function(x) mean(x))) %>%
  tidyr::unnest(newprop)

# multiliner

bar <- ps %>%
  dplyr::mutate(x = purrr::map2(prop, means, function(x, y) x * dnorm(dat, mean = y, sd = sds))) %>%
  dplyr::mutate(sumx = purrr::map(x, function(x) sum(x))) %>%
  tidyr::unnest(sumx) %>%
  dplyr::mutate(probs = purrr::map2(x, sumx, function(x, y) x / y)) %>% #calculate probability
  dplyr::mutate(newmean = purrr::map(probs, function(x) sum(x * dat) / sum(x))) %>%
  tidyr::unnest(newmean)



for( i in 1:10 ) {

  if(i == 1){
    mus <- ps$means
    taus <- ps$prop
  }

  T_1 <- taus[1] * dnorm( dat, mus[1] )
  T_2 <- taus[2] * dnorm( dat, mus[2] )
  T_3 <- taus[3] * dnorm( dat, mus[3] )

  T_sum <- sum(T_1, T_2, T_3)

  P_1 <- T_1 / T_sum
  P_2 <- T_2 / T_sum ## note: P_2 = 1 - P_1
  P_3 <- T_3 / T_sum

  taus <- c(mean(P_1), mean(P_2), mean(P_3))

  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters?

  mus <- c(sum( P_1 * dat ) / sum(P_1), sum( P_2 * dat ) / sum(P_2), sum( P_3 * dat ) / sum(P_3))

  ## print the current estimates

  temp_res <- tibble::tibble(means = mus, props = taus)

  print(temp_res)

}


####
x <- foo


# modified sum only considers finite values
sum.finite <- function(x) {
  sum(x[is.finite(x)])
}

mem <- kmeans(x,2)$cluster
mu1 <- mean(x[mem==1])
mu2 <- mean(x[mem==2])
sigma1 <- sd(x[mem==1])
sigma2 <- sd(x[mem==2])
pi1 <- sum(mem==1)/length(mem)
pi2 <- sum(mem==2)/length(mem)

Q <- 0
# starting value of expected value of the log likelihood
Q[2] <- sum.finite(log(pi1)+log(dnorm(x, mu1, sigma1))) + sum.finite(log(pi2)+log(dnorm(x, mu2, sigma2)))

k <- 2

while (abs(Q[k]-Q[k-1])>=1e-6) {
  # E step
  comp1 <- pi1 * dnorm(x, mu1, sigma1)
  comp2 <- pi2 * dnorm(x, mu2, sigma2)
  comp.sum <- comp1 + comp2

  p1 <- comp1/comp.sum
  p2 <- comp2/comp.sum

  # M step
  pi1 <- sum.finite(p1) / length(x)
  pi2 <- sum.finite(p2) / length(x)

  mu1 <- sum.finite(p1 * x) / sum.finite(p1)
  mu2 <- sum.finite(p2 * x) / sum.finite(p2)

  sigma1 <- sqrt(sum.finite(p1 * (x-mu1)^2) / sum.finite(p1))
  sigma2 <- sqrt(sum.finite(p2 * (x-mu2)^2) / sum.finite(p2))

  p1 <- pi1
  p2 <- pi2

  k <- k + 1
  Q[k] <- sum(log(comp.sum))
}
