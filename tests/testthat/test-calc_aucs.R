options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning:
# test-calc_aucs.R:16: warning: get_predictions works
# partial argument match of 'contrasts' to 'contrasts.arg'

predictions_sm <- c(
  9.163243e-03, 5.502478e-06, 9.929243e-01, 7.991857e-09,
  4.979207e-01, 3.774758e-15, 9.998627e-01, 2.159155e-06,
  1.789605e-05, 2.774999e-02, 1.743478e-09, 8.567441e-01
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
  expect_equal(calc_auroc(predictions_sm, outcomes_sm), 0.9)
  expect_equal(calc_auroc(predictions_med, outcomes_med), 0.5)
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_sm, outcomes_sm), 0.7123179,
    tolerance = tol
  )
  expect_equal(calc_auprc(predictions_med, outcomes_med), 0.5649431,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_sm, test_data_sm, "dx", "cancer"),
    list(auroc = 0.9, auprc = 0.7123179),
    tolerance = tol
  )
})
