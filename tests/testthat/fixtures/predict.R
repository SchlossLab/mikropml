library(dplyr)
devtools::load_all()
doFuture::registerDoFuture()
future::plan(future::multicore, workers = 8)

get_sensspec_seed <- function(seed) {
  ml_result <- run_ml(otu_mini_bin, 'glmnet', seed = seed)
  sensspec <- get_model_sensspec(ml_result$trained_model,
                                 ml_result$test_data,
                                 'dx', 'cancer') %>%
                                 mutate(seed = seed)
  return(sensspec)
}
sensspec_dat <- furrr::future_map_dfr(seq(100, 110), get_sensspec_seed,
                                      .options = furrr::furrr_options(seed = TRUE))
saveRDS(sensspec_dat, testthat::test_path('fixtures', 'sensspec_dat.Rds'))
