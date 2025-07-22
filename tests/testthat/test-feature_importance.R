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
      perf_metric_diff = -2.10526316379855e-08,
      pvalue = 1,
      lower = 0.647368421052632,
      upper = 0.647368421052632
    ),
    tolerance = 10e-5
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
      perf_metric_diff = -2.10526316379855e-08,
      pvalue = 1,
      lower = 0.647368421052632,
      upper = 0.647368421052632
    ),
    tolerance = 10e-5
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
      perf_metric_diff = 0.00805261052631578,
      pvalue = 0.188118811881188,
      lower = 0.621052631578947,
      upper = 0.655263157894737
    ),
    tolerance = 10e-4
  )
})


test_that("feature importances are correct", {
  set.seed(2019)
  # eps <- if (capabilities("long.double")) sqrt(.Machine$double.eps) else 0.1 # https://blog.r-hub.io/2019/05/21/nold/
  expect_equal(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    ),
    structure(
      list(
        perf_metric = c(
          0.629157894736842, 0.605473684210526,
          0.63878947368421, 0.636763157894737, 0.629447368421053, 0.637868421052632,
          0.642552631578947, 0.592157894736842, 0.639684210526316, 0.637526315789474
        ),
        perf_metric_diff = c(
          0.0182105263157895, 0.0418947368421053,
          0.00857894736842113, 0.0106052631578948, 0.0179210526315789,
          0.00950000000000006, 0.00481578947368422, 0.0552105263157895,
          0.00768421052631585, 0.00984210526315787
        ),
        pvalue = c(
          0.237623762376238,
          0.099009900990099, 0.188118811881188, 0.376237623762376, 0.386138613861386,
          0.356435643564356, 0.287128712871287, 0.118811881188119, 0.217821782178218,
          0.435643564356436
        ),
        lower = c(
          0.581578947368421, 0.531578947368421,
          0.618421052631579, 0.581578947368421, 0.494736842105263, 0.594736842105263,
          0.623684210526316, 0.521052631578947, 0.623684210526316, 0.555263157894737
        ),
        upper = c(
          0.668421052631579, 0.657894736842105, 0.652631578947368,
          0.694736842105263, 0.718421052631579, 0.678947368421053, 0.657894736842105,
          0.660526315789474, 0.652631578947368, 0.721052631578947
        ),
        feat = structure(1:10, levels = c(
          "Otu00001",
          "Otu00002", "Otu00003", "Otu00004", "Otu00005", "Otu00006", "Otu00007",
          "Otu00008", "Otu00009", "Otu00010"
        ), class = "factor"),
        method = c(
          "glmnet",
          "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet", "glmnet",
          "glmnet", "glmnet"
        ),
        perf_metric_name = c(
          "AUC", "AUC", "AUC",
          "AUC", "AUC", "AUC", "AUC", "AUC", "AUC", "AUC"
        ),
        seed = c(
          2019,
          2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019
        )
      ),
      row.names = c(NA, -10L), class = "data.frame"
    ),
    tolerance = 0.1 # https://blog.r-hub.io/2019/05/21/nold/
  )
})
test_that("feature importances are correct when tibbles used", {
  skip_on_cran()
  skip_on_ci()
  set.seed(2019)
  expect_snapshot(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      corr_thresh = 1
    )
  )
})

test_that("custom grouped features works", {
  skip_on_cran()
  skip_on_ci()
  set.seed(2019)
  feats <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
    dplyr::rename(dx = .outcome) %>%
    split_outcome_features(., "dx") %>%
    .$features
  groups <- group_correlated_features(feats, corr_thresh = 0.5)

  expect_snapshot(
    get_feature_importance(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data %>%
        dplyr::as_tibble(),
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019,
      groups = groups
    )
  )
})
test_that("empirical confidence interval works", {
  x <- 1:10000
  alpha <- 0.05
  lower <- lower_bound(x, alpha)
  upper <- upper_bound(x, alpha)
  expect_equal(lower, 250)
  expect_equal(upper, 9750)
  expect_equal(length(c(x[1:lower - 1], x[upper:length(x)])) / length(x),
    alpha,
    tolerance = 0.01
  )
})
