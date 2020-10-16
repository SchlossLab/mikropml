
test_that("get_outcome_type works", {
  expect_equal(get_outcome_type(c(1, 2, 1)), "continuous")
  expect_equal(get_outcome_type(c("a", "b", "b")), "binary")
  expect_equal(get_outcome_type(c("a", "b", "c")), "multiclass")
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
  expect_equal(calc_perf_metrics(otu_mini_results1$test_data, otu_mini_results1$trained_model, "dx", caret::multiClassSummary, class_probs = TRUE), unlist(c(otu_mini_results1$performance[, !(colnames(otu_mini_results1$performance) %in% c("cv_metric_AUC", "method", "seed"))])))
})
