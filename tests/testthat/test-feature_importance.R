# tests for functions in get_features_importance
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'


# find_permuted_auc
test_that("permuted auc returns correct value for non-correlated feature", {
  expect_equal(
    find_permuted_auc(trained_model_mini, test_data_sm, "dx", "Otu00049", "cancer"),
    c(auc = 0.571052631578947, auc_diff = 0)
  )
})

test_that("permuted auc returns correct value for [fake] correlated feature", {
  expect_equal(
    find_permuted_auc(
      trained_model_mini,
      test_data_sm,
      "dx",
      "Otu00049|Otu00050",
      "cancer"
    ),
    c(auc = 0.571052631578947, auc_diff = 0)
  )
})

feat_imps <- structure(list(
  auc = c(0.548052631578947, 0.516078947368421, 0.580815789473684),
  auc_diff = c(0.023, 0.0549736842105263, -0.0097631578947368),
  names = structure(1:3, .Label = c("Otu00001", "Otu00002", "Otu00003"), class = "factor"),
  method = c("regLogistic", "regLogistic", "regLogistic"),
  seed = c(2019, 2019, 2019)
),
class = "data.frame", row.names = c(NA, -3L)
)
# get_feature_importance
test_that("feature importances are correct", {
  expect_equal(get_feature_importance(
    trained_model_mini,
    train_data_mini,
    test_data_mini,
    "dx",
    "cancer",
    "regLogistic",
    seed = 2019,
    corr_thresh = 1
  ), feat_imps, tolerance = 1e-5)
})
