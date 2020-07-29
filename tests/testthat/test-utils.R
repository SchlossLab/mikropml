test_df <- data.frame(
  outcome = c("normal", "normal", "cancer"),
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
  expect_equal(check_outcome_column(test_df, NA), "outcome")
  expect_error(
    check_outcome_column(test_df, "not_a_column"),
    "Outcome 'not_a_column' not in column names of data."
  )
})

test_that("pick_outcome_value works", {
  expect_equal(pick_outcome_value(test_df, "outcome", "fewer"), "cancer")
  expect_equal(pick_outcome_value(test_df, "outcome", "first"), "normal")
  expect_error(pick_outcome_value(test_df, "outcome", "not_a_method"), "Method not_a_method for selecting outcome value not recognized.")
})

test_that("check_outcome_value works", {
  expect_equal(check_outcome_value(test_df, "outcome", "cancer"), "cancer")
  expect_equal(
    check_outcome_value(test_df, "outcome", NA, method = "fewer"),
    "cancer"
  )
  expect_equal(
    check_outcome_value(test_df, "outcome", NA, method = "first"),
    "normal"
  )
  expect_error(
    check_outcome_value(test_df, "outcome", NA, method = "not_a_method"),
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
})

test_that("randomize_feature_order works", {
  reordered_df <- data.frame(
    outcome = c("normal", "normal", "cancer"),
    var2 = 4:6,
    var1 = 1:3
  )
  expect_equal(
    randomize_feature_order(test_df, "outcome", seed = 20),
    reordered_df
  )
})

test_that("validate_permute works", {
  expect_true(is.null(validate_permute(TRUE)))
  expect_true(is.null(validate_permute(FALSE)))
  expect_error(
    validate_permute("not_a_logical"),
    "`permute` must be TRUE or FALSE"
  )
})

test_that("validate_nfolds works", {
  expect_true(is.null(validate_nfolds(as.integer(1), test_df)))
  expect_error(
    validate_nfolds(as.integer(10), test_df),
    "`nfolds` must be an integer"
  )
  expect_error(
    validate_nfolds(as.integer(0), test_df),
    "`nfolds` must be an integer"
  )
  expect_error(
    validate_nfolds("not_an_int", test_df),
    "`nfolds` must be an integer"
  )
})

test_that("validate_training_frac works", {
  expect_true(is.null(validate_training_frac(0.8)))
  expect_error(
    validate_training_frac("not_a_number"),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_error(
    validate_training_frac(1),
    "`training_frac` must be a numeric between 0 and 1."
  )
  expect_error(
    validate_training_frac(0),
    "`training_frac` must be a numeric between 0 and 1."
  )
})

test_that("validate_seed works", {
  expect_true(is.null(validate_seed(NA)))
  expect_true(is.null(validate_seed(10)))
  expect_error(
    validate_seed("not_a_number"),
    "`seed` must be `NA` or numeric."
  )
})
