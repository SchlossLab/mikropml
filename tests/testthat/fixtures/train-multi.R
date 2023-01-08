library(dplyr)
devtools::load_all()
doFuture::registerDoFuture()
future::plan(future::multicore, workers = 8)

otu_data_preproc <- mikropml::otu_data_preproc$dat_transformed

results_list <- future.apply::future_lapply(seq(100, 102), function(seed) {
    run_ml(otu_data_preproc, "glmnet", seed = seed)
}, future.seed = TRUE)
saveRDS(results_list, testthat::test_path("fixtures", "results_list.Rds"))

param_grid <- expand.grid(
    seeds = seq(100, 110),
    methods = c("glmnet", "rf")
)
results_mtx <- future.apply::future_mapply(
  function(seed, method) {
    run_ml(otu_data_preproc, method, seed = seed, find_feature_importance = TRUE)
  },
  param_grid$seeds,
  param_grid$methods %>% as.character(),
  future.seed = TRUE
)
saveRDS(results_mtx, testthat::test_path("fixtures", "results_mtx.Rds"))
