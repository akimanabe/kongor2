# generate_sample <- function(n, ages, p = c(300, 0.6, 0)) {
# }

  ages <- 7
  n <- 500
  p = c(300, 0.6, 0)

  p[1] * (1 - exp(-p[2]*(ages - p[3])))

  L_t <- c(NA)

  for(i in 1:ages+1){
    L_t[i] <- p[1] * (1 - exp(-p[2]*(i - p[3])))
  }

  M <- 0.4
  N_t <- c(NA)
  for(i in 0:ages){
    if(i == 0){N_t[i] <- n}
  N_t[i] <- N_t[i-1] * exp(-M*i)
  }

