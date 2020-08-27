test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
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

test_that("check_dataset works", {
  expect_true(is.null(check_dataset(test_df)))
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
  expect_error(
    check_method("not_a_method"),
    "Method 'not_a_method' is not supported. Supported methods are:"
  )
  expect_true(is.null(check_method("regLogistic")))
})

test_that("check_outcome_column works", {
  expect_equal(check_outcome_column(test_df, NULL), "outcome")
  expect_error(
    check_outcome_column(test_df, "not_a_column"),
    "Outcome 'not_a_column' not in column names of data."
  )
})

test_that("check_outcome_value works", {
  expect_equal(check_outcome_value(test_df, "outcome", "cancer"), "cancer")
  expect_equal(
    check_outcome_value(test_df, "outcome", NULL, method = "fewer"),
    "cancer"
  )
  expect_equal(
    check_outcome_value(test_df, "outcome", NULL, method = "first"),
    "normal"
  )
  expect_error(
    check_outcome_value(test_df, "outcome", NULL, method = "not_a_method"),
    "Method not_a_method for selecting outcome value not recognized."
  )
  expect_message(
    check_outcome_value(test_df, "outcome", "cancer"),
    "Using 'outcome' as the outcome column and 'cancer' as the outcome value of interest."
  )
  expect_error(
    check_outcome_value(test_df, "outcome", "not_an_outcome"),
    "No rows in the outcome column "
  )
  expect_error(
    check_outcome_value(test_df_na, "outcome", "cancer"),
    "Missing data in the output variable is not allowed, but the outcome variable has"
  )
  expect_warning(
    check_outcome_value(test_df_empty, "outcome", "cancer"),
    "Possible missing data in the output variable: "
  )
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
  expect_true(is.null(check_kfold(1, test_df)))
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
})

test_that("check_seed works", {
  expect_true(is.null(check_seed(NULL)))
  expect_true(is.null(check_seed(10)))
  expect_error(
    check_seed("not_a_number"),
    "`seed` must be `NULL` or numeric."
  )
})

test_that("check_all works", {
  expect_true(is.null(check_all(otu_small, "regLogistic", TRUE, as.integer(5), 0.8, NULL)))
})

test_that("check if package is installed", {
  expect_equal(check_package_installed("caret"), TRUE)
  expect_equal(check_package_installed("asdf"), FALSE)
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
