# tests for functions in get_features_importance

# find_permuted_auc
test_that("permuted auc returns correct value for non-correlated feature", {
  expect_equal(
    find_permuted_auc(trained_model_sm1, test_data_sm, "dx", "Otu00049", "cancer"),
    c(auc = 0.48573684, auc_diff = 0.01952632)
  )
})

test_that("permuted auc returns correct value for [fake] correlated feature", {
  expect_equal(
    find_permuted_auc(
      trained_model_sm1,
      test_data_sm,
      "dx",
      "Otu00049|Otu00050",
      "cancer"
    ),
    c(auc = 0.5061578947, auc_diff = -0.0008947368)
  )
})

feat_imps <- structure(list(auc = c(
  0.548052631578947, 0.516078947368421,
  0.580815789473684
), auc_diff = c(0.023, 0.0549736842105263, -0.0097631578947368), names = structure(1:3, .Label = c(
  "Otu00001", "Otu00002",
  "Otu00003"
), class = "factor")), class = "data.frame", row.names = c(
  NA,
  -3L
))
# get_feature_importance
test_that("feature importances are correct", {
  expect_equal(get_feature_importance(
    trained_model_mini,
    train_data_mini,
    test_data_mini,
    "dx",
    "cancer"
  ), feat_imps, tolerance = 1e-5)
})
