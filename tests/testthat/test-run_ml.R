options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)
# Without this, underlying code in either stats or base R causes this warning in several places:
#   warning: get_predictions works
#   partial argument match of 'contrasts' to 'contrasts.arg'

test_hyperparams <- structure(list(
  param = c(
    "lambda", "lambda", "lambda", "alpha",
    "sigma", "sigma", "C", "C", "maxdepth", "maxdepth", "nrounds",
    "gamma", "eta", "max_depth", "colsample_bytree", "min_child_weight",
    "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-3", "1e-2", "1e-1", "1", "0.00000001", "0.0000001", "0.01", "0.1",
    "1", "2", "10", "0", "0.01", "1", "0.8", "1", "0.4", "1", "2"
  ),
  method = c(
    "glmnet", "glmnet", "glmnet", "glmnet",
    "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -19L)
)


get_all_but_model <- function(ml_results) {
  return(ml_results[names(ml_results) != "trained_model"])
}

expect_equal_ml_results <- function(result1, result2, tol = 1e-5) {
  return(
    eval(bquote(expect_equal(get_all_but_model(result1),
      get_all_but_model(result2),
      tolerance = tol
    )))
  )
}

otu_mini_group <- c(
  "A", "E", "B", "E", "E", "A", "E", "D", "C", "B", "A", "E",
  "B", "A", "C", "C", "D", "E", "C", "D", "E", "A", "D", "A", "D",
  "D", "A", "B", "E", "D", "A", "D", "E", "B", "E", "A", "B", "A",
  "E", "A", "D", "A", "D", "A", "C", "A", "B", "B", "E", "A", "E",
  "B", "C", "D", "D", "C", "A", "E", "E", "B", "B", "A", "C", "D",
  "D", "D", "D", "A", "D", "C", "A", "D", "D", "B", "C", "E", "C",
  "E", "C", "B", "D", "B", "D", "C", "B", "B", "B", "B", "B", "B",
  "B", "C", "D", "D", "E", "A", "E", "D", "E", "A", "D", "A", "E",
  "E", "C", "B", "B", "E", "B", "C", "C", "D", "A", "A", "E", "E",
  "C", "A", "C", "E", "A", "D", "A", "C", "D", "E", "E", "A", "A",
  "B", "E", "C", "B", "B", "C", "C", "D", "C", "E", "E", "E", "C",
  "E", "D", "D", "B", "B", "B", "E", "E", "A", "A", "A", "B", "D",
  "B", "D", "B", "B", "B", "D", "B", "B", "D", "B", "D", "C", "C",
  "B", "A", "A", "D", "C", "E", "E", "A", "B", "B", "A", "B", "A",
  "B", "E", "A", "C", "E", "A", "A", "E", "C", "C", "C", "B", "D",
  "D", "B", "B", "E", "D", "D"
)

test_that("run_ml works for L2 logistic regression with grouping & feature importance", {
  expect_equal_ml_results(
    expect_warning(run_ml(otu_small[, 1:4], # use built-in hyperparameters
      "glmnet",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 2
    ), "The model didn't converge in some cross-validation folds because it is predicting something close to a constant. This means that certain performance metrics can't be calculated, and suggests that some of the hyperparameters being used are doing very poorly."),
    otu_mini_results1
  )
})

test_that("run_ml works for linear regression", {
  expect_equal_ml_results(
    expect_message(expect_warning(run_ml(otu_mini[, 2:4], # use built-in hyperparameters
      "glmnet",
      outcome_colname = "Otu00001",
      find_feature_importance = TRUE,
      seed = 2019,
      kfold = 2,
      cv_times = 2
    ), "Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")),
    otu_mini_cont_results1
  )
})

test_that("run_ml works for random forest", {
  # set.seed(2019)
  # otu_mini_group <- sample(LETTERS[1:5], nrow(otu_mini), replace = TRUE)
  expect_equal_ml_results( # use built-in hyperparams function
    mikropml::run_ml(otu_mini,
      "rf",
      outcome_colname = "dx",
      find_feature_importance = TRUE,
      seed = 2019,
      kfold = 2,
      cv_times = 2,
      groups = otu_mini_group
    ),
    otu_mini_results2,
    tol = 1e-3
  )
})

test_that("run_ml works for svmRadial", {
  expect_equal_ml_results(
    expect_warning(mikropml::run_ml(otu_mini,
      "svmRadial",
      outcome_colname = "dx",
      hyperparameters = test_hyperparams %>% get_hyperparams_from_df("svmRadial"),
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 2
    )),
    otu_mini_results3
  )
})

test_that("run_ml works for xgbTree", {
  skip_on_os(c("linux", "windows")) # bug in xgboost package: https://discuss.xgboost.ai/t/colsample-by-tree-leads-to-not-reproducible-model-across-machines-mac-os-windows/1709
  expect_equal_ml_results(
    mikropml::run_ml(
      otu_mini,
      "xgbTree",
      outcome_colname = "dx",
      hyperparameters = test_hyperparams %>% get_hyperparams_from_df("xgbTree"),
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 2
    ),
    otu_mini_results4,
    tol = 1e-3
  )
})

test_that("run_ml works for rpart2", {
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini,
      "rpart2",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 2
    ),
    otu_mini_results5
  )
})

test_that("run_ml errors for unsupported method", {
  expect_error(expect_warning(
    run_ml(
      otu_small,
      "not_a_method"
    ),
    "Method 'not_a_method' is not officially supported by mikropml."
  ), "method 'not_a_method' is not supported.")
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
