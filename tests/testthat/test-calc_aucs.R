options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

predictions_sm <- c(
  0.302553723266671, 0.21761199572356, 0.149449241881401, 0.0733187260761979,
  0.275953437442721, 0.166145147735239, 0.275792077306705, 0.160381104457551,
  0.311538843510942, 0.137316276051265, 0.118099504411304, 0.293299235679477
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
  expect_equal(calc_auroc(predictions_sm, outcomes_sm), 0.7)
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_sm, outcomes_sm), 0.2337616,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_sm1, test_data_sm, "dx", "cancer"),
    list(auroc = 0.7, auprc = 0.2337616),
    tolerance = tol
  )
})
