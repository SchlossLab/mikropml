
test_that("get_outcome_type works", {
  expect_equal(get_outcome_type(c(1,2,1)), 'continuous')
  expect_equal(get_outcome_type(c('a','b','b')), 'binary')
  expect_equal(get_outcome_type(c('a','b','c')), 'multiclass')
})

test_that("get_perf_metric_fn works", {
  expect_equal(get_perf_metric_fn('continuous'), caret::defaultSummary)
  expect_equal(get_perf_metric_fn('binary'), caret::twoClassSummary)
  expect_equal(get_perf_metric_fn('multiclass'), caret::multiClassSummary)
  expect_error(get_perf_metric_fn('asdf'), "Outcome type of outcome must be one of:")
})

test_that("get_perf_metric_name works", {
  expect_equal(get_perf_metric_name('continuous'), "RMSE")
  expect_equal(get_perf_metric_name('binary'), "ROC")
  expect_equal(get_perf_metric_name('multiclass'), "logLoss")
  expect_error(get_perf_metric_name('asdf'), "Outcome type of outcome must be one of:")
})
