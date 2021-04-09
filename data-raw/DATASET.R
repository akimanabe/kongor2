## code to prepare `DATASET` dataset goes here

freqsample1 <- readr::read_csv("./data-raw/freqsample1.csv")

usethis::use_data(freqsample1, overwrite = TRUE)
