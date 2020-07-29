# tests for functions in get_features_importance
# group_correlated_features
test_that("correlated groups correct", {
  corr <- dplyr::tibble(
    feature1 = c("A", "C", "D"),
    feature2 = c("B", "A", "E")
  )
  test_data <- dplyr::tibble(
    inf = NA,
    A = NA,
    B = NA,
    C = NA,
    D = NA,
    E = NA,
    F = NA
  )
  expect_equal(sort(group_correlated_features(corr, test_data)), c("B|A|C", "E|D", "F"))
})
test_that("no correlated groups correct", {
  corr <- dplyr::tibble(feature1 = c(character()), feature2 = character())
  test_data <- dplyr::tibble(
    inf = NA,
    A = NA,
    B = NA,
    C = NA,
    D = NA,
    E = NA,
    F = NA
  )
  expect_equal(
    sort(group_correlated_features(corr, test_data)),
    c("A", "B", "C", "D", "E", "F")
  )
})
test_that("empty dataframe correct", {
  corr <- dplyr::tibble(feature1 = c(character()), feature2 = character())
  test_data <- dplyr::tibble()
  expect_equal(sort(group_correlated_features(corr, test_data)), c("NA", "NA"))
})

# find_permuted_auc
test_that("permuted auc returns correct value for non-correlated feature", {
  fn_output <- find_permuted_auc(trained_model_sm, test_data_sm, "dx", "Otu00049", "cancer")
  expect_equal(fn_output, c(auc = 0.9, auc_diff = 0))
})

test_that("permuted auc returns correct value for [fake] correlated feature", {
  fn_output <- find_permuted_auc(
    trained_model_sm,
    test_data_sm,
    "dx",
    "Otu00049|Otu00050",
    "cancer"
  )
  expect_equal(fn_output, c(auc = 0.9, auc_diff = 0))
})

feat_imps <- structure(list(
  auc = c(
    0.7955, 0.9065, 0.9045, 0.9, 0.9, 0.9,
    0.8815, 0.906, 0.9465, 0.8935, 0.927, 0.9295, 0.9, 0.9085, 0.903,
    0.903, 0.9105, 0.903, 0.8975, 0.803, 0.907, 0.9, 0.9, 0.9, 0.93,
    0.8875, 0.938, 0.9005, 0.9, 0.9035, 0.9, 0.9, 0.9085, 0.8395,
    0.9045, 0.882, 0.9035, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9,
    0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9,
    0.9, 0.9
  ),
  auc_diff = c(
    0.1045, -0.00649999999999999, -0.00449999999999999,
    0, 0, 0, 0.0185, -0.00599999999999997, -0.0465, 0.00650000000000001,
    -0.027, -0.0295, 0, -0.00849999999999999, -0.003, -0.003, -0.0105,
    -0.00299999999999999, 0.00250000000000001, 0.097, -0.00699999999999997,
    0, 0, 0, -0.03, 0.0125, -0.038, -0.00049999999999999, 0, -0.0035,
    0, 0, -0.00849999999999999, 0.0605, -0.00449999999999999, 0.018,
    -0.0035, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9.99315105491838e-18,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  names = structure(1:60, .Label = c(
    "Otu00001",
    "Otu00002", "Otu00003", "Otu00004", "Otu00005", "Otu00006", "Otu00007",
    "Otu00008", "Otu00009", "Otu00010", "Otu00011", "Otu00012", "Otu00013",
    "Otu00014", "Otu00015", "Otu00016", "Otu00017", "Otu00018", "Otu00019",
    "Otu00020", "Otu00021", "Otu00022", "Otu00023", "Otu00024", "Otu00025",
    "Otu00026", "Otu00027", "Otu00028", "Otu00029", "Otu00030", "Otu00031",
    "Otu00032", "Otu00033", "Otu00034", "Otu00035", "Otu00036", "Otu00037",
    "Otu00038", "Otu00039", "Otu00040", "Otu00041", "Otu00042", "Otu00043",
    "Otu00044", "Otu00045", "Otu00046", "Otu00047", "Otu00048", "Otu00049",
    "Otu00050", "Otu00051", "Otu00052", "Otu00053", "Otu00054", "Otu00055",
    "Otu00056", "Otu00057", "Otu00058", "Otu00059", "Otu00060"
  ), class = "factor")
),
class = "data.frame", row.names = c(NA, -60L)
)
# get_feature_importance
test_that("feature importances are correct", {
  f_imps <- get_feature_importance(
    train_data_sm,
    trained_model_sm,
    test_data_sm,
    "dx",
    "cancer"
  )
  expect_equal(f_imps, feat_imps)
})
