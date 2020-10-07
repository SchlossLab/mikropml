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
test_that("find_permuted_perf_metric works", {
  expect_equal(
    find_permuted_perf_metric(test_data_sm, trained_model_sm1, "dx", multiClassSummary, "AUC", "Otu00049"),
    c(perf_metric = 0.644315789, perf_metric_diff = 0.003052632)
  )
  expect_equal(
    find_permuted_perf_metric(test_data_sm, trained_model_sm1, "dx", multiClassSummary, "AUC", "Otu00049|Otu00050"),c(perf_metric = 0.655184211, perf_metric_diff = -0.007815789))
})

feat_imps <- structure(list(
  perf_metric = c(0.548052631578947, 0.516078947368421, 0.580815789473684),
  perf_metric_diff = c(0.023, 0.0549736842105263, -0.0097631578947368),
  names = structure(1:3, .Label = c("Otu00001", "Otu00002", "Otu00003"), class = "factor"),
  method = c("regLogistic", "regLogistic", "regLogistic"),
  perf_metric_name = rep('AUC',3),
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
    multiClassSummary,
    'AUC',
    "regLogistic",
    seed = 2019,
    corr_thresh = 1
  ), feat_imps, tolerance = 1e-5)
})
