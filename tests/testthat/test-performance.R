test_that("get_outcome_type works", {
  expect_equal(get_outcome_type(c(1, 2, 1)), "continuous")
  expect_equal(get_outcome_type(c("a", "b", "b")), "binary")
  expect_equal(get_outcome_type(c("a", "b", "c")), "multiclass")
})
test_that("get_outcome_type errors when num_outcomes < 2", {
  error_msg <- "A continuous, binary, or multi-class outcome variable is required, but this dataset has "
  expect_error(
    get_outcome_type(c(1, 1)),
    error_msg
  )
  expect_error(
    get_outcome_type(c("a", "a", "a")),
    error_msg
  )
  expect_error(
    get_outcome_type(c()),
    error_msg
  )
})

test_that("get_perf_metric_fn works", {
  expect_equal(get_perf_metric_fn("continuous"), caret::defaultSummary)
  expect_equal(get_perf_metric_fn("binary"), caret::multiClassSummary)
  expect_equal(get_perf_metric_fn("multiclass"), caret::multiClassSummary)
  expect_error(get_perf_metric_fn("asdf"), "Outcome type of outcome must be one of:")
})

test_that("get_perf_metric_name works", {
  expect_equal(get_perf_metric_name("continuous"), "RMSE")
  expect_equal(get_perf_metric_name("binary"), "AUC")
  expect_equal(get_perf_metric_name("multiclass"), "logLoss")
  expect_error(get_perf_metric_name("asdf"), "Outcome type of outcome must be one of:")
})

test_that("calc_perf_metrics works", {
  expect_equal(
    calc_perf_metrics(otu_mini_bin_results_glmnet$test_data,
      otu_mini_bin_results_glmnet$trained_model,
      "dx",
      caret::multiClassSummary,
      class_probs = TRUE
    ),
    unlist(c(otu_mini_bin_results_glmnet$performance[, !(colnames(otu_mini_bin_results_glmnet$performance) %in% c("cv_metric_AUC", "method", "seed"))]))
  )
})

test_that("get_performance_tbl works", {
  set.seed(2019)
  expect_equal(
    get_performance_tbl(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "AUC",
      TRUE,
      "glmnet",
      seed = 2019
    ),
    otu_mini_bin_results_glmnet$performance
  )
  expect_warning(
    get_performance_tbl(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data,
      "dx",
      caret::multiClassSummary,
      "not_a_perf_metric",
      TRUE,
      "glmnet",
      seed = 2019
    ),
    "The performance metric provided does not match the metric used to train the data."
  )
})

test_that("calc_perf_bootstrap_split returns consistent results", {
  ml_result <- otu_mini_bin_results_glmnet
  outcome_colname <- "dx"
  model <- ml_result$trained_model
  test_dat <- ml_result$test_data
  outcome_type <- get_outcome_type(test_dat %>% dplyr::pull(outcome_colname))
  class_probs <- outcome_type != "continuous"
  method <- model$modelInfo$label
  seed <- ml_result$performance %>% dplyr::pull(seed)
  perf_metric_function <- get_perf_metric_fn(outcome_type)
  perf_metric_name <- model$metric
  class_probs <- outcome_type != "continuous"
  method <- model$trained_model$modelInfo$label

  set.seed(20230129)
  split_1 <- rsample::bootstraps(test_dat, times = 3) %>%
    dplyr::pull(splits) %>%
    .[[1]]
  perf_1 <- calc_perf_bootstrap_split(
    split_1, model, outcome_colname,
    perf_metric_function,
    perf_metric_name,
    class_probs,
    method,
    seed
  )
  set.seed(20230129)
  split_2 <- rsample::bootstraps(test_dat, times = 3) %>%
    dplyr::pull(splits) %>%
    .[[1]]
  perf_2 <- calc_perf_bootstrap_split(
    split_2, model, outcome_colname,
    perf_metric_function,
    perf_metric_name,
    class_probs,
    method,
    seed
  )

  expect_equal(perf_1, perf_2)
})

test_that("bootstrap_performance returns consistent results", {
  set.seed(20230129)
  expect_warning(
    boot_1 <- bootstrap_performance(
      otu_mini_bin_results_glmnet,
      "dx",
      bootstrap_times = 1,
      alpha = 1
    ),
    "Recommend at least 1000 non-missing bootstrap resamples for terms"
  )
  boot_2 <- structure(
    list(
      term = c(
        "Accuracy", "AUC", "Balanced_Accuracy",
        "cv_metric_AUC", "Detection_Rate", "F1", "Kappa", "logLoss",
        "Neg_Pred_Value", "Pos_Pred_Value", "prAUC", "Precision", "Recall",
        "Sensitivity", "Specificity"
      ),
      .lower = c(
        0.512820512820513,
        0.592391304347826, 0.520380434782609, 0.622173713235294, 0.282051282051282,
        0.536585365853659, 0.0389105058365759, 0.688040379015192, 0.428571428571429,
        0.611111111111111, 0.524734824159823, 0.611111111111111, 0.478260869565217,
        0.478260869565217, 0.5625
      ),
      .estimate = c(
        0.512820512820513,
        0.592391304347826, 0.520380434782609, 0.622173713235294, 0.282051282051282,
        0.536585365853659, 0.0389105058365759, 0.688040379015192, 0.428571428571429,
        0.611111111111111, 0.524734824159823, 0.611111111111111, 0.478260869565217,
        0.478260869565217, 0.5625
      ),
      .upper = c(
        0.512820512820513, 0.592391304347826,
        0.520380434782609, 0.622173713235294, 0.282051282051282, 0.536585365853659,
        0.0389105058365759, 0.688040379015192, 0.428571428571429, 0.611111111111111,
        0.524734824159823, 0.611111111111111, 0.478260869565217, 0.478260869565217,
        0.5625
      ),
      .alpha = c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1
      ),
      .method = c(
        "percentile", "percentile", "percentile", "percentile",
        "percentile", "percentile", "percentile", "percentile", "percentile",
        "percentile", "percentile", "percentile", "percentile", "percentile",
        "percentile"
      )
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -15L)
  )
  expect_equal(boot_1 %>% dplyr::arrange(term), boot_2 %>% dplyr::arrange(term))
})

test_that("sensspec calculations work", {
  expect_equal(
    calc_model_sensspec(
      otu_mini_bin_results_glmnet$trained_model,
      otu_mini_bin_results_glmnet$test_data,
      "dx"
    ),
    readRDS(
      testthat::test_path("fixtures", "otu_mini_bin_results_glmnet_sensspec.Rds")
    )
  )
  sensspec_dat <- readRDS(test_path("fixtures", "sensspec_dat.Rds"))
  expect_equal(
    calc_mean_roc(sensspec_dat),
    readRDS(testthat::test_path("fixtures", "sensspec_roc.Rds"))
  )
  expect_equal(
    calc_mean_prc(sensspec_dat),
    readRDS(testthat::test_path("fixtures", "sensspec_prc.Rds"))
  )
})

test_that("calc_baseline_precision works", {
  expect_equal(
    calc_baseline_precision(otu_mini_bin, "dx", "cancer"),
    0.49
  )
  expect_equal(
    calc_baseline_precision(otu_mini_bin, "dx", "normal"),
    0.51
  )
  expect_equal(
    data.frame(y = c("a", "b", "a", "b")) %>%
      calc_baseline_precision("y", "a"),
    0.50
  )
  expect_equal(data.frame(y = c("a", rep.int('b', 4))) %>%
                   calc_baseline_precision(outcome_colname = "y", pos_outcome = "a"),
               0.2)
  expect_error(
    data.frame(y = c("a")) %>%
      calc_baseline_precision("y", "a"),
    "A binary or multi-class outcome variable is required"
  )
  expect_error(
    data.frame(y = c("b")) %>%
      calc_baseline_precision("y", "a"),
    "A binary or multi-class outcome variable is required"
  )
})
test_that('calc_balanced_precision', {
    # when precision is already balanced
    expect_equal(calc_balanced_precision(0.3, 0.5), 0.3)
    expect_equal(calc_balanced_precision(0.9, 0.5), 0.9)

    # other situations
    expect_equal(calc_balanced_precision(0.2, 0.2), 0.5)
})
