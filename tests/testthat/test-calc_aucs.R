options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

predictions_sm <- c(
  0.00916324268270163, 5.50247841835105e-06, 0.992924252456899,
  7.99185706501504e-09, 0.497920659124481, 3.77475828372553e-15,
  0.999862709044069, 2.15915501822206e-06, 1.78960513393944e-05,
  0.0277499865819482, 1.74347802861519e-09, 0.856744054489971
)

outcomes_sm <- c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
tol <- 1e-5

test_that("get_predictions works", {
  expect_equal(get_predictions(trained_model_sm1, test_data_sm, "cancer"),
    predictions_sm,
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
})
test_that("calc_auroc works", {
  expect_equal(calc_auroc(predictions_sm, outcomes_sm), 0.9)
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_sm, outcomes_sm), 0.7123179,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_sm1, test_data_sm, "dx", "cancer"),
    list(auroc = 0.9, auprc = 0.7123179),
    tolerance = tol
  )
})
