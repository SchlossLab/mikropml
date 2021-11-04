test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_df_novar <- data.frame(
  outcome = c("normal", "normal", "normal"),
  var1 = 1:3,
  var2 = 4:6
)

test_df_na <- data.frame(
  outcome = c("normal", NA, "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_df_empty <- data.frame(
  outcome = c("", "", "cancer"),
  var1 = 1:3,
  var2 = 4:6
)

test_df_numeric <- data.frame(
  outcome = c(0, 1, 2),
  var1 = 1:3,
  var2 = 4:6
)

test_that("check_dataset works", {
  expect_true(is.null(check_dataset(test_df)))
  expect_error(
    check_dataset("not_a_df"),
    "The dataset must be a `data.frame` or `tibble`"
  )
  expect_error(
    check_dataset(data.frame(outcome = c(), var1 = c())),
    "No rows detected in dataset."
  )
  expect_error(
    check_dataset(data.frame(outcome = 1:3)),
    "1 or fewer columns detected in dataset. There should be an outcome column and at least one feature column."
  )
})

test_that("check_method works", {
  expect_warning(
    check_method("not_a_method"),
    "Method 'not_a_method' is not officially supported by mikropml. However, this method might work in our pipeline. You can use the caret documentation to see what hyperparameters are required. Supported methods are:"
  )
  expect_null(check_method("glmnet"))
})

test_that("check_outcome_column works", {
  expect_equal(expect_message(check_outcome_column(test_df, NULL), "Using"), "outcome")
  expect_error(
    check_outcome_column(test_df, "not_a_column"),
    "Outcome 'not_a_column' not in column names of data."
  )
})

test_that("check_outcome_value works", {
  expect_null(check_outcome_value(test_df, "outcome"))
  expect_error(
    check_outcome_value(test_df_na, "outcome"),
    "Missing data in the output variable is not allowed, but the outcome variable has"
  )
  expect_warning(
    check_outcome_value(test_df_empty, "outcome"),
    "Possible missing data in the output variable: "
  )
  expect_error(check_outcome_value(test_df_novar, "outcome"), "A binary or multi-class outcome variable is required, but this dataset has")
  expect_null(expect_warning(check_outcome_value(test_df_numeric, "outcome"), "Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values."))
})

test_that("check_permute works", {
  expect_true(is.null(check_permute(TRUE)))
  expect_true(is.null(check_permute(FALSE)))
  expect_error(
    check_permute("not_a_logical"),
    "`permute` must be TRUE or FALSE"
  )
})

test_that("check_kfold works", {
  expect_true(is.null(check_kfold(2, test_df)))
  expect_error(check_kfold(1, test_df), "`kfold` must be an integer between 1 and the number of features in the data.")
  expect_error(
    check_kfold(10, test_df),
    "`kfold` must be an integer"
  )
  expect_error(
    check_kfold(0, test_df),
    "`kfold` must be an integer"
  )
  expect_warning(expect_error(
    check_kfold("not_an_int", test_df),
    "`kfold` must be an integer"
  ), "NAs introduced by coercion")
})

test_that("check_training_frac works", {
  expect_true(is.null(check_training_frac(0.8)))
  expect_error(
    check_training_frac("not_a_number"),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_error(
    check_training_frac(1),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_error(
    check_training_frac(0),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_warning(
    check_training_frac(0.499),
    "`training_frac` is less than 0.5. The training set will be smaller than the testing set."
  )
})
test_that("check_training_indices works", {
  dat <- data.frame(a = 1:3, b = 2:4)
  expect_warning(
    check_training_indices(c(2.8, 1), dat),
    "The training indices vector contains non-integer numbers."
  )
  expect_error(
    check_training_indices(c(1, 12312, 1), dat),
    "The training indices vector contains a value that is too large"
  )
  expect_error(
    check_training_indices(c(-1, 2, 3), dat),
    "The training indices vector contains a value less than 1."
  )
  expect_error(
    check_training_indices(c(1:5), dat),
    "The training indices vector contains too many values for the size of the dataset."
  )
})
test_that("check_seed works", {
  expect_true(is.null(check_seed(NA)))
  expect_true(is.null(check_seed(10)))
  expect_error(
    check_seed("not_a_number"),
    "`seed` must be `NA` or numeric."
  )
})

test_that("check_all works", {
  expect_null(check_all(otu_small, "glmnet", TRUE, as.integer(5), 0.8, NULL, NULL, NULL, NULL, NULL, NA))
})

test_that("check_packages_installed works", {
  expect_equal(all(check_packages_installed("caret")), TRUE)
  expect_equal(all(check_packages_installed("this_is_not_a_package")), FALSE)
  expect_equal(all(check_packages_installed("caret", "this_is_not_a_package")), FALSE)
  expect_equal(all(check_packages_installed(c("caret", "this_is_not_a_package"))), FALSE)
})

test_that("check_features works", {
  expect_true(is.null(check_features(test_df)))
  expect_true(is.null(check_features(dplyr::as_tibble(test_df))))
  expect_error(check_features(NULL))
  expect_true(is.null(check_features(test_df_na, check_missing = FALSE)))
  expect_true(is.null(expect_warning(check_features(test_df_empty), "ossible missing data in the features: ")))
  expect_error(
    check_features(test_df_na, check_missing = TRUE),
    "Missing data in the features is not allowed, but the features have"
  )
})

test_that("check_groups works", {
  expect_null(check_groups(mikropml::otu_mini_bin, NULL, 2))
  expect_null(check_groups(mikropml::otu_mini_bin, sample(LETTERS, nrow(mikropml::otu_mini_bin), replace = T), 2))
  expect_error(check_groups(mikropml::otu_mini_bin, c(1, 2), 2), "group should be a vector that is the same length as the number of rows in the dataset")
  expect_error(check_groups(mikropml::otu_mini_bin, data.frame(x = c(1, 2)), 2), "group should be either a vector or NULL, but group is class")
  expect_error(check_groups(mikropml::otu_mini_bin, c(rep(1, 199), NA), 2), "No NA values are allowed in group, but ")
  expect_error(check_groups(mikropml::otu_mini_bin, c(rep(1, 200)), 2), "The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`")
  expect_error(check_groups(mikropml::otu_mini_bin, c(rep(1, 199), 2), 5), "The number of folds for cross-validation, `k-fold`, must be less than the number of groups. Number of groups: ")
})

test_that("check_corr_thresh works", {
  expect_null(check_corr_thresh(1))
  expect_null(check_corr_thresh(0.8))
  expect_null(check_corr_thresh(NULL))
  expect_error(check_corr_thresh(2019), "`corr_thresh` must be `NULL` or numeric between 0 and 1 inclusive.
    You provided: ")
  expect_error(check_corr_thresh(corr_thresh = "a"), "`corr_thresh` must be `NULL` or numeric between 0 and 1 inclusive.
    You provided:")
})

test_that("check_perf_metric_function works", {
  expect_null(check_perf_metric_function(caret::defaultSummary))
  expect_null(check_perf_metric_function(NULL))
  expect_error(check_perf_metric_function("a"), "`perf_metric_function` must be `NULL` or a function.
    You provided:")
})

test_that("check_perf_metric_name works", {
  expect_null(check_perf_metric_name("a"))
  expect_null(check_perf_metric_name(NULL))
  expect_error(check_perf_metric_name(1), "`perf_metric_name` must be `NULL` or a character\n    You provided: 1")
})

test_that("check_cat_feats works", {
  expect_null(check_cat_feats(test_df[, 2:3]))
  expect_error(check_cat_feats(test_df), "No categorical features can be used when performing permutation importance. Please change these features to numeric. One option is to use `preprocess_data`.")
})

test_that("check_remove_var works", {
  expect_null(check_remove_var(NULL))
  expect_null(check_remove_var("nzv"))
  expect_error(check_remove_var("asdf"), "`remove_var` must be one of: NULL, 'nzv','zv'. You provided:")
})

test_that("check_ntree works", {
  expect_null(check_ntree(NULL))
  expect_null(check_ntree(1000))
  expect_error(check_ntree("asdf"), "`ntree` must be of length 1 and class numeric. You provided: ")
  expect_error(check_ntree(-10), "`ntree` must be greater than zero. You provided: ")
  expect_error(check_ntree(c(0, 1)), "`ntree` must be of length 1 and class numeric. You provided: ")
})

test_that("abort_packages_not_installed works", {
  testfun <- function(...) abort_packages_not_installed(...)
  expect_null(testfun("utils"))
  expect_error(
    testfun("not_a_package"),
    "The following package\\(s\\) are required for `testfun\\(\\)` but are not installed:"
  )
})
