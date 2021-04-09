# # First let there be a sampledata
#
# foodata
#
# # Then, let k means method select intial_mus
#
#
#
# tibble::tibble(n_modes = c(1:10)) %>%
#   dplyr::mutate(
#     init_mean = purrr::map(
#       n_modes,
#       function(x) data.frame(kmeans(foodata, x)$centers)$Length %>% sort()
#       )
#     ) %>%
#   dplyr::select(init_mean) %>%
#   purrr::flatten() %>%
#   purrr::pluck(.,1)
#
# #https://tinyheero.github.io/2016/01/03/gmm-em.html
