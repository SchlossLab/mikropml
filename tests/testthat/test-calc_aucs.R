options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

predictions_ans <- c(
  0.376699598098026, 0.361412503485008, 0.424361915961239, 0.572987036892158,
  0.462575617454478, 0.495662609551985, 0.268665237275343, 0.513620970112174,
  0.448559956511362, 0.572757252948955, 0.507821600082324, 0.500394115811712,
  0.585712754751816, 0.500802090895146, 0.489966970527271, 0.401401679912621,
  0.40502306096355, 0.609201623624306, 0.456018223836579, 0.482073461806217,
  0.577447665088213, 0.344579461426511, 0.484460016356973, 0.453267401269666,
  0.548543300840126, 0.203324356793987, 0.487083618876598, 0.499204988451657,
  0.483808225317108, 0.419368467026678, 0.41259850228935, 0.523451660001951,
  0.534953904484442, 0.468589713277824, 0.529414526946646, 0.41993171421088,
  0.503126382256404, 0.548831997199401, 0.560270238107935
)
outcomes_ans <- c(
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,
  1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1
)
tol <- 1e-5

test_that("get_predictions works", {
  expect_equal(get_predictions(trained_model_mini, test_data_mini, "cancer"),
    predictions_ans,
    tolerance = tol
  )
})
test_that("recode_outcome works", {
  expect_equal(
    recode_outcome(test_data_mini, "dx", "cancer"),
    outcomes_ans
  )
  expect_equal(
    recode_outcome(test_data_mini, "dx", "normal"),
    c(
      1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0,
      0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0
    )
  )
})
test_that("calc_auroc works", {
  expect_equal(calc_auroc(predictions_ans, outcomes_ans), 0.5710526,
    tolerance = tol
  )
})
test_that("calc_auprc works", {
  expect_equal(calc_auprc(predictions_ans, outcomes_ans), 0.5438684,
    tolerance = tol
  )
})
test_that("calc_aucs works", {
  expect_equal(
    calc_aucs(trained_model_mini, test_data_mini, "dx", "cancer"),
    list(auroc = 0.571052631578947, auprc = 0.543868353382751),
    tolerance = tol
  )
})
