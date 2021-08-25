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
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049",
      0.6473684
    ),
    c(
      perf_metric = 0.647368421052632,
      perf_metric_diff = -2.1052632526164e-08
    ),
    tol = 10e-5
  )
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00049|Otu00050",
      0.6473684
    ),
    c(
      perf_metric = 0.647368421052632,
      perf_metric_diff = -2.1052632526164e-08
    ),
    tol = 10e-5
  )
  set.seed(2019)
  expect_equal(
    find_permuted_perf_metric(
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      otu_mini_bin_results_glmnet$trained_model,
      "dx", caret::multiClassSummary,
      "AUC", TRUE, "Otu00009",
      0.6473684
    ),
    c(
      perf_metric = 0.639315789473684,
      perf_metric_diff = 0.00805261052631601
    ),
    tol = 10e-5
  )
})

feat_imp <- structure(list(perf_metric = c(
  0.629157894736842, 0.605473684210526,
  0.638789473684211, 0.636763157894737, 0.629447368421053, 0.637868421052632,
  0.642552631578947, 0.592157894736842, 0.639684210526316, 0.637526315789474
), perf_metric_diff = c(
  0.0182105263157896, 0.0418947368421053,
  0.00857894736842102, 0.0106052631578948, 0.0179210526315789,
  0.00950000000000006, 0.00481578947368433, 0.0552105263157895,
  0.00768421052631585, 0.00984210526315799
), names = structure(1:10, .Label = c(
  "Otu00001",
  "Otu00002", "Otu00003", "Otu00004", "Otu00005", "Otu00006", "Otu00007",
  "Otu00008", "Otu00009", "Otu00010"
), class = "factor"), method = c(
  "glmnet",
  "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
  "glmnet", "glmnet"
), perf_metric_name = c(
  "AUC", "AUC", "AUC",
  "AUC", "AUC", "AUC", "AUC", "AUC", "AUC", "AUC"
), seed = c(
  2019,
  2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019
)), row.names = c(
  NA,
  -10L
), class = "data.frame")

test_that("feature importances are correct", {
  set.seed(2019)
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
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
  set.seed(2019)
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
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

test_that("custom grouped features works", {
  skip_on_cran()
  set.seed(2019)
  feats <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
    dplyr::rename(dx = .outcome) %>%
    split_outcome_features(., "dx") %>%
    .$features
  groups <- group_correlated_features(feats, corr_thresh = 0.5)

  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$trained_model$trainingData %>%
        dplyr::rename(dx = .outcome),
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      groups = groups
    ),
    structure(list(perf_metric = c(
      0.629157894736842, 0.596921052631579,
      0.633605263157895, 0.639105263157895, 0.642421052631579, 0.596842105263158,
      0.640289473684211, 0.629868421052632
    ), perf_metric_diff = c(
      0.0182105263157896,
      0.0504473684210527, 0.0137631578947369, 0.00826315789473686,
      0.00494736842105259, 0.0505263157894738, 0.00707894736842096,
      0.0175000000000001
    ), names = structure(1:8, .Label = c(
      "Otu00001",
      "Otu00002|Otu00003|Otu00005", "Otu00004", "Otu00006", "Otu00007",
      "Otu00008", "Otu00009", "Otu00010"
    ), class = "factor"), method = c(
      "glmnet",
      "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet"
    ), perf_metric_name = c(
      "AUC", "AUC", "AUC", "AUC", "AUC", "AUC",
      "AUC", "AUC"
    ), seed = c(
      2019, 2019, 2019, 2019, 2019, 2019, 2019,
      2019
    )), row.names = c(NA, -8L), class = "data.frame")
  )
})
