perf_df_untidy <- structure(list(cv_auroc = c(0.611904862414009, 0.566911585365854, 0.569763211382114, 0.598345528455285, 0.561239524702939),
                          test_auroc = c(0.471052631578947, 0.602631578947368, 0.621052631578947, 0.45, 0.573684210526316),
                          test_auprc = c(0.487051223328847, 0.559779555464991, 0.641450500683036, 0.443692491218808, 0.547463343122288),
                          method = c("regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic"),
                          seed = 100:104), row.names = c(NA, -5L),
                     class = c("tbl_df", "tbl", "data.frame"))
perf_df_tidy <- structure(list(method = c("regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic", "regLogistic"),
                               metric = c("cv_auroc", "test_auroc", "test_auprc", "cv_auroc", "test_auroc", "test_auprc", "cv_auroc", "test_auroc", "test_auprc", "cv_auroc", "test_auroc", "test_auprc", "cv_auroc", "test_auroc", "test_auprc"),
                               value = c(0.611904862414009, 0.471052631578947, 0.487051223328847, 0.566911585365854, 0.602631578947368, 0.559779555464991, 0.569763211382114, 0.621052631578947, 0.641450500683036, 0.598345528455285, 0.45, 0.443692491218808, 0.561239524702939, 0.573684210526316, 0.547463343122288),
                               metric_pretty = c("Cross-validation AUROC", "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC", "Cross-validation AUROC", "Testing AUROC", "Testing AUPRC")),
                          row.names = c(NA, -15L),
                          class = c("tbl_df", "tbl", "data.frame"))
test_that("tidy_perf_data works", {
  expect_equal(tidy_perf_data(perf_df_untidy), perf_df_tidy)
})
