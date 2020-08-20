options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

predictions_sm <- c(9.707251e-02, 3.755703e-01, 2.372989e-02, 9.114752e-01, 8.721929e-01, 1.492075e-05, 7.998901e-01, 8.084112e-01, 9.978739e-01, 3.135994e-01,9.011613e-01, 8.973311e-01, 4.743989e-01, 3.261513e-01, 7.670219e-01, 1.101077e-03, 2.539591e-01, 1.196996e-01, 2.459846e-01, 9.634875e-01, 9.982137e-01, 9.299074e-02, 9.345718e-02, 8.624414e-01, 8.346058e-01,2.630144e-02, 1.839343e-01, 9.137120e-01, 2.205726e-02, 5.216090e-01, 4.293018e-01, 6.287346e-01, 2.273751e-01, 7.503992e-02, 2.090081e-01,4.608598e-01, 3.452780e-01, 2.244180e-01, 2.844962e-02)

outcomes_sm <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1,1)
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
    c(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
  )
})
test_that("calc_auroc works", {
  expect_equal(calc_auroc(predictions_sm, outcomes_sm), 0.5052632,
  tolerance = tol
  )
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_sm, outcomes_sm), 0.5447323,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_sm1, test_data_sm, "dx", "cancer"),
    list(auroc = 0.5052632, auprc = 0.5447323),
    tolerance = tol
  )
})
