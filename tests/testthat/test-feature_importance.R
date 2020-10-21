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
    find_permuted_perf_metric(otu_mini_results1$test_data,
      otu_mini_results1$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049",
      seed = 2019
    ),
    c(perf_metric = 0.5157895, perf_metric_diff = 0.0000000),
    tol = 10e-5
  )
  expect_equal(
    find_permuted_perf_metric(otu_mini_results1$test_data,
      otu_mini_results1$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049|Otu00050",
      seed = 2019
    ),
    c(perf_metric = 0.5157895, perf_metric_diff = 0.0000000),
    tol = 10e-5
  )
})

test_that("feature importances are correct", {
  expect_equal(
    get_feature_importance(
      otu_mini_results1$trained_model,
      otu_mini_results1$trained_model$trainingData %>% dplyr::rename(dx = .outcome),
      otu_mini_results1$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    data.frame(
      perf_metric = c(0.510947368421053, 0.542552631578947, 0.491026315789474),
      perf_metric_diff = c(0.00484210526315784, -0.0267631578947369, 0.0247631578947368),
      names = structure(c(1L, 3L, 2L), .Label = c("Otu00001", "Otu00002", "Otu00003"), class = "factor"),
      method = c("glmnet", "glmnet", "glmnet"),
      perf_metric_name = c("AUC", "AUC", "AUC"),
      seed = c(2019, 2019, 2019)
    )
  )
})
