# tests for functions in get_features_importance
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'


# find_permuted_perf_metric
test_that("find_permuted_perf_metric works", {
  expect_equal(
    find_permuted_perf_metric(test_data_mini, trained_model_mini, "dx", caret::multiClassSummary, "AUC", TRUE, "Otu00049", seed = 2019),
    c(perf_metric = 0.5710526, perf_metric_diff = 0.0000000), tol = 10e-5
  )
  expect_equal(
    find_permuted_perf_metric(test_data_mini, trained_model_mini, "dx", caret::multiClassSummary, "AUC", TRUE, "Otu00049|Otu00050", seed = 2019),c(perf_metric = 0.5710526, perf_metric_diff = 0.0000000), tol = 10e-5)
})

test_that("feature importances are correct", {
  expect_equal(get_feature_importance(
    trained_model_mini,
    train_data_mini,
    test_data_mini,
    "dx",
    caret::multiClassSummary,
    'AUC',
    TRUE,
    "glmnet",
    seed = 2019,
    corr_thresh = 1
  ), structure(list(perf_metric = c(0.551578947368421, 0.512447368421053, 
                                    0.574421052631579), perf_metric_diff = c(0.0194736842105263, 
                                                                             0.0586052631578947, -0.00336842105263156), names = structure(1:3, .Label = c("Otu00001", 
                                                                                                                                                          "Otu00002", "Otu00003"), class = "factor"), method = c("glmnet", 
                                                                                                                                                                                                                 "glmnet", "glmnet"), perf_metric_name = c("AUC", "AUC", 
                                                                                                                                                                                                                                                                     "AUC"), seed = c(2019, 2019, 2019)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                              -3L)))
})
