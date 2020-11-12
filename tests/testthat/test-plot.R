perf_df_untidy <- structure(list(
  cv_auroc = c(0.611904862414009, 0.566911585365854, 0.569763211382114, 0.598345528455285, 0.561239524702939),
  test_auroc = c(0.471052631578947, 0.602631578947368, 0.621052631578947, 0.45, 0.573684210526316),
  test_auprc = c(0.487051223328847, 0.559779555464991, 0.641450500683036, 0.443692491218808, 0.547463343122288),
  method = c("glmnet", "glmnet", "glmnet", "glmnet", "glmnet"),
  seed = 100:104
),
row.names = c(NA, -5L),
class = c("tbl_df", "tbl", "data.frame")
)
perf_df_tidy <- structure(list(
  method = c(
    "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
    "glmnet", "glmnet"
  ),
  metric = c(
    "Cross-validation AUROC",
    "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC",
    "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC",
    "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC",
    "Testing AUROC", "Testing AUPRC"
  ),
  value = c(
    0.611904862414009,
    0.471052631578947, 0.487051223328847, 0.566911585365854, 0.602631578947368,
    0.559779555464991, 0.569763211382114, 0.621052631578947, 0.641450500683036,
    0.598345528455285, 0.45, 0.443692491218808, 0.561239524702939,
    0.573684210526316, 0.547463343122288
  )
),
row.names = c(NA, -15L),
class = c("tbl_df", "tbl", "data.frame")
)
test_that("tidy_perf_data works", {
  expect_equal(tidy_perf_data(perf_df_untidy), perf_df_tidy)
})

test_that("get_hp_performance works", {
  expect_equal(get_hp_performance(otu_mini_bin_results_glmnet$trained_model),
               list(dat = structure(list(alpha = c(0L, 0L, 0L, 0L, 0L, 0L),
                                         lambda = c(1e-04, 0.001, 0.01, 0.1, 1, 10), AUC = c(0.608255208333333,
                                                                                             0.608255208333333, 0.608645833333333, 0.616678921568627,
                                                                                             0.622173713235294, 0.618740808823529)), row.names = c(NA,
                                                                                                                                                   6L), class = "data.frame"), params = "lambda", metric = "AUC")
               )
})

test_that("combine_hp_performance works", {
  expect_equal(combine_hp_performance(list(otu_mini_bin_results_glmnet$trained_model, otu_mini_bin_results_glmnet$trained_model)),
               list(dat = structure(list(alpha = c(0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                   0L, 0L, 0L, 0L, 0L), lambda = c(1e-04, 0.001, 0.01, 0.1, 1, 10,
                                                                                   1e-04, 0.001, 0.01, 0.1, 1, 10), AUC = c(0.608255208333333, 0.608255208333333,
                                                                                                                            0.608645833333333, 0.616678921568627, 0.622173713235294, 0.618740808823529,
                                                                                                                            0.608255208333333, 0.608255208333333, 0.608645833333333, 0.616678921568627,
                                                                                                                            0.622173713235294, 0.618740808823529)), row.names = c(NA, -12L
                                                                                                                            ), class = "data.frame"), params = "lambda", metric = "AUC")
               )
})
