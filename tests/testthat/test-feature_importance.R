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
  expect_equal(
    find_permuted_auc(trained_model_sm1, test_data_sm, "dx", "Otu00049", "cancer"),
    c(auc = 0.7075, auc_diff = -0.0075)
  )
})

test_that("permuted auc returns correct value for [fake] correlated feature", {
  expect_equal(find_permuted_auc(
    trained_model_sm1,
    test_data_sm,
    "dx",
    "Otu00049|Otu00050",
    "cancer"
  ), 
  c(auc = 0.71, auc_diff = -0.01))
})

feat_imps <- structure(list(
  auc = c(
    0.7065, 0.8155, 0.708, 0.71, 0.744, 0.7,
    0.7755, 0.728, 0.784, 0.7285, 0.735, 0.7755, 0.7005, 0.8085,
    0.7, 0.7325, 0.7105, 0.7, 0.8585, 0.7055, 0.7235, 0.7, 0.7, 0.7,
    0.7035, 0.716, 0.7, 0.863, 0.7, 0.7065, 0.7, 0.7, 0.702, 0.599,
    0.7165, 0.7655, 0.7, 0.703, 0.7, 0.7, 0.7, 0.7025, 0.7, 0.7,
    0.7, 0.7, 0.7, 0.7145, 0.7075, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7,
    0.7, 0.7, 0.7, 0.7, 0.7
  ),
  auc_diff = c(
    -0.00650000000000005,
    -0.1155, -0.00800000000000001, -0.01, -0.044, 0, -0.0755, -0.028,
    -0.084, -0.0285, -0.035, -0.0755000000000001, -5e-04, -0.1085,
    0, -0.0325, -0.0105, 0, -0.1585, -0.00550000000000002, -0.0235,
    0, 0, 0, -0.0035, -0.016, 0, -0.163, 0, -0.00650000000000001,
    0, 0, -0.002, 0.101, -0.0165, -0.0655, 0, -0.003, 0, 0, 0, -0.0025,
    0, 0, 0, 0, 0, -0.0145, -0.00750000000000001, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0
  ),
  names = structure(1:60,
    .Label = c(
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
    ),
    class = "factor"
  )
),
class = "data.frame", row.names = c(NA, -60L)
)
# get_feature_importance
test_that("feature importances are correct", {
  expect_equal(get_feature_importance(
    trained_model_sm1,
    train_data_sm,
    test_data_sm,
    "dx",
    "cancer"
  ), feat_imps)
})
