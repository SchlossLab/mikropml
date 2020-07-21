## code to prepare `default_hyperparams` dataset
default_hyperparams <- readr::read_csv("data-raw/default_hyperparams.csv")
usethis::use_data(default_hyperparams, overwrite = TRUE)
