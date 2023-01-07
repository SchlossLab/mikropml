library(dplyr)
devtools::load_all()

results_mtx <- readRDS(testthat::test_path('fixtures', 'results_mtx.Rds'))
get_sensspec_seed <- function(colnum) {
    result <- results_mtx[, colnum]
    trained_model <- result$trained_model
    test_data <- result$test_data
    seed <- result$performance$seed
    method <- result$trained_model$method
    sensspec <- calc_model_sensspec(trained_model,
                                   test_data,
                                   'dx', 'cancer') %>%
        mutate(seed = seed, method = method)
    return(sensspec)
}
sensspec_dat <- purrr::map_dfr(seq(1, dim(results_mtx)[2]),
                               get_sensspec_seed)
saveRDS(sensspec_dat, testthat::test_path('fixtures', 'sensspec_dat.Rds'))
saveRDS(calc_mean_prc(sensspec_dat), testthat::test_path('fixtures', 'sensspec_prc.Rds'))
saveRDS(calc_mean_roc(sensspec_dat), testthat::test_path('fixtures', 'sensspec_roc.Rds'))

saveRDS(
    calc_model_sensspec(
        otu_mini_bin_results_glmnet$trained_model,
        otu_mini_bin_results_glmnet$test_data,
        'dx',
        'cancer'
    ),
    testthat::test_path('fixtures', 'otu_mini_bin_results_glmnet_sensspec.Rds')
)
