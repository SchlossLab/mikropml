options(warnPartialMatchArgs = FALSE)
# Without this, underlying code in either stats or base R causes this warning:
#   test-calc_aucs.R:16: warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

get_all_but_model <- function(ml_results) {
  return(ml_results[names(ml_results) != "trained_model"])
}

expect_equal_ml_results <- function(result1, result2, tol = 1e-5) {
  return(expect_equal(get_all_but_model(result1),
    get_all_but_model(result2),
    tolerance = tol
  ))
}

test_that("run_ml works for regLogistic", {
  expect_equal_ml_results(
    mikRopML::run_ml(otu_small,
      "regLogistic",
      outcome_colname = "dx",
      outcome_value = "cancer",
      hyperparameters = mikRopML::default_hyperparams,
      find_feature_importance = FALSE,
      seed = 2019
    ),
    otu_sm_results1
  )
  expect_equal_ml_results(
    mikRopML::run_ml(otu_mini,
      "regLogistic",
      outcome_colname = "dx",
      outcome_value = "cancer",
      hyperparameters = mikRopML::default_hyperparams,
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = as.integer(2)
    ),
    otu_mini_results1
  )
})

test_that("run_ml errors for unsupported method", {
  expect_error(
    run_ml(
      otu_small,
      "not_a_method"
    ),
    "Method 'not_a_method' is not supported. Supported methods are:"
  )
})
test_that("run_ml errors if outcome_colname not in dataframe", {
  expect_error(
    run_ml(
      otu_small,
      "rf",
      outcome_colname = "not_a_colname"
    ),
    "Outcome 'not_a_colname' not in column names of data."
  )
})
test_that("run_ml errors if outcome_value not in outcome column", {
  expect_error(
    run_ml(
      otu_small,
      "rf",
      outcome_colname = "dx",
      outcome_value = "not_an_outcome"
    ),
    "No rows in the outcome column"
  )
})
test_that("run_ml errors if outcome is not binary", {
  expect_error(
    run_ml(
      data.frame(
        dx = c("cancer", "adenoma", "normal"),
        otu1 = 1:3,
        otu2 = 4:6
      ),
      "rf",
      outcome_colname = "dx",
      kfold = as.integer(2)
    ),
    "A binary outcome variable is required, but this dataset has 3 outcomes"
  )
})
