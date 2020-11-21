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
  skip_on_cran()
  expect_equal(
    find_permuted_perf_metric(otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049",
      seed = 2019
    ),
    c(perf_metric = 0.6473684, perf_metric_diff = 0.0000000),
    tol = 10e-5
  )
  skip_on_cran()
  expect_equal(
    find_permuted_perf_metric(otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049|Otu00050",
      seed = 2019,
      0.6473684
    ),
    c(perf_metric = 0.6473684, perf_metric_diff = 0.0000000),
    tol = 10e-5
  )
  skip_on_cran()
  expect_equal(
    find_permuted_perf_metric(otu_mini_bin_results_glmnet$test_data %>% dplyr::as_tibble(),
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00009",
      seed = 2019,
      0.6473684
    ),
    c(perf_metric = 0.639105263, perf_metric_diff = 0.008263158),
    tol = 10e-5
  )
})

feat_imp <- structure(list(perf_metric = c(
  0.639105263157895, 0.644236842105263,
  0.631447368421053, 0.632236842105263, 0.596921052631579, 0.639526315789474,
  0.639578947368421, 0.607657894736842, 0.643789473684211, 0.635605263157895
), perf_metric_diff = c(
  0.00826315789473686, 0.00313157894736845,
  0.015921052631579, 0.0151315789473684, 0.0504473684210527, 0.00784210526315793,
  0.00778947368421055, 0.0397105263157895, 0.00357894736842107,
  0.0117631578947369
), names = structure(c(
  9L, 5L, 10L, 1L, 8L,
  4L, 3L, 2L, 7L, 6L
), .Label = c(
  "Otu00001", "Otu00002", "Otu00003",
  "Otu00004", "Otu00005", "Otu00006", "Otu00007", "Otu00008", "Otu00009",
  "Otu00010"
), class = "factor"), method = c(
  "glmnet", "glmnet",
  "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
  "glmnet"
), perf_metric_name = c(
  "AUC", "AUC", "AUC", "AUC", "AUC",
  "AUC", "AUC", "AUC", "AUC", "AUC"
), seed = c(
  2019, 2019, 2019,
  2019, 2019, 2019, 2019, 2019, 2019, 2019
)), class = "data.frame", row.names = c(
  NA,
  -10L
))

test_that("feature importances are correct", {
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>% dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    feat_imp
  )
})
test_that("feature importances are correct when tibbles used", {
  skip_on_cran()
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>% dplyr::rename(dx = .outcome) %>% dplyr::as_tibble(),
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    feat_imp
  )
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>% dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data %>% dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    feat_imp
  )
})
