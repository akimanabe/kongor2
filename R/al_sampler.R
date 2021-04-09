al_sample <-
  function(n, ages, ps, lambda, sdlevel = 10) {
    means <-
      fishgr::vb(age = ages,
                        ps = ps) %>%
      round()

    sds <- abs(means / sdlevel)

    tibble::tibble(ages, means, sds, lambda) %>%
      dplyr::mutate(
        Length = purrr::map2(means, sds,
                             function(x, y) rnorm(n, mean = x, sd = y)),
        lambda = lambda / sum(lambda),
        lambda_N = ceiling(n * lambda),
        Length_N = purrr::map2(Length, lambda_N,
                               function(x, y) sample(x, y, replace = F))
      ) %>%
      dplyr::select(ages, means, sds, lambda, Length_N) %>%
      tidyr::unnest(cols = c(Length_N)) %>%
      dplyr::mutate(ages = factor(ages)) %>%
      dplyr::rename(Length = Length_N) %>%
      dplyr::sample_n(N)
    }
