options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning:
# test-calc_aucs.R:16: warning: get_predictions works
# partial argument match of 'contrasts' to 'contrasts.arg'

predictions_sm <- c(
  2.044805e-03, 1.846221e-04, 4.331578e-03, 1.290319e-05,
  9.939288e-01, 6.812802e-05, 9.983625e-01, 1.208421e-04,
  4.403648e-03, 2.486293e-01, 1.853760e-05, 3.086007e-03
)
predictions_med <- c(
  0.003330436, 0.935320188, 0.003146670, 0.050361065,
  0.037302455, 0.225469013, 0.002964700, 0.002525344,
  0.006217845, 0.132653677, 0.187598964, 0.002360271
)
outcomes_sm <- c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
outcomes_med <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
tol <- 1e-5

test_that("get_predictions works", {
  expect_equal(get_predictions(trained_model_sm, test_data_sm, "cancer"),
    predictions_sm,
    tolerance = tol
  )
  expect_equal(get_predictions(trained_model1, test_data1, "cancer"),
    predictions_med,
    tolerance = tol
  )
})
test_that("recode_outcome works", {
  expect_equal(
    recode_outcome(test_data_sm, "dx", "cancer"),
    outcomes_sm
  )
  expect_equal(
    recode_outcome(test_data_sm, "dx", "normal"),
    c(1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1)
  )
  expect_equal(
    recode_outcome(test_data1, "dx", "cancer"),
    outcomes_med
  )
})
test_that("calc_auroc works", {
  expect_equal(calc_auroc(predictions_sm, outcomes_sm), 1)
  expect_equal(calc_auroc(predictions_med, outcomes_med), 0.5)
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_sm, outcomes_sm), 1)
  expect_equal(calc_auprc(predictions_med, outcomes_med), 0.5649431,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_sm, test_data_sm, "dx", "cancer"),
    list(auroc = 1, auprc = 1)
  )
})
