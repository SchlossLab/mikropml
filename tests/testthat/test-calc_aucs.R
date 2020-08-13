options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning:
#   test-calc_aucs.R:16: warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

predictions_mini <- c(1.000000e+00, 9.999999e-01, 1.000000e+00, 9.999946e-01, 9.947294e-01, 0.000000e+00, 6.851176e-06, 1.000000e+00, 1.000000e+00, 4.197609e-06, 1.554312e-15, 4.432876e-11)
outcomes_mini <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
tol <- 1e-5

test_that("get_predictions works", {
  expect_equal(get_predictions(trained_model_mini, test_data_mini, "cancer"),
    predictions_mini,
    tolerance = tol
  )
})
test_that("recode_outcome works", {
  expect_equal(
    recode_outcome(test_data_mini, "dx", "cancer"),
    outcomes_mini
  )
  expect_equal(
    recode_outcome(test_data_mini, "dx", "normal"),
    c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  )
})
test_that("calc_auroc works", {
  expect_equal(calc_auroc(predictions_mini, outcomes_mini), 0.525)
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_mini, outcomes_mini), 0.2035579,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_mini, test_data_mini, "dx", "cancer"),
    list(auroc = 0.5, auprc = 0.1730928),
    tolerance = tol
  )
})
