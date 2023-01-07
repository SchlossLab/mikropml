library(dplyr)
devtools::load_all()
doFuture::registerDoFuture()
future::plan(future::multicore, workers = 8)

otu_data_preproc <- preprocess_data(otu_mini_bin, "dx")$dat_transformed
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
