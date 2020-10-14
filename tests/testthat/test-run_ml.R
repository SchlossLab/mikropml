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
    "cost", "cost", "cost", "epsilon", "loss",
    "sigma", "sigma", "C", "C", "maxdepth", "maxdepth", "nrounds",
    "gamma", "eta", "max_depth", "colsample_bytree", "min_child_weight",
    "subsample", "mtry", "mtry"
  ),
  value = c(
    "1e-3", "1e-2", "1e-1", "0.01", "L2_primal", "0.00000001", "0.0000001", "0.01", "0.1",
    "1", "2", "10", "0", "0.01", "1", "0.8", "1", "0.4", "1", "2"
  ),
  method = c(
    "regLogistic", "regLogistic", "regLogistic", "regLogistic",
    "regLogistic", "svmRadial", "svmRadial", "svmRadial", "svmRadial",
    "rpart2", "rpart2", "xgbTree", "xgbTree", "xgbTree", "xgbTree",
    "xgbTree", "xgbTree", "xgbTree", "rf", "rf"
  )
),
class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, -20L)
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

test_that("run_ml works for L2 logistic regression", {
  hparams_list <- test_hyperparams %>% get_hyperparams_from_df("regLogistic")
  hparams_list[["epsilon"]] <- as.numeric(hparams_list[["epsilon"]])
  set.seed(2019)
  expect_equal_ml_results(
    run_ml(otu_mini, # use built-in hyperparameters
      "regLogistic",
      outcome_colname = "dx",
      find_feature_importance = TRUE,
      seed = 2019,
      kfold = 2,
      cv_times = 2,
      group = sample(LETTERS[1:10], nrow(otu_mini), replace = TRUE)
    ),
    otu_mini_results1
  )
})

test_that("run_ml works for linear regression", {
  hparams_list <- test_hyperparams %>% get_hyperparams_from_df("glmnet")
  set.seed(2019)
  expect_equal_ml_results(
    run_ml(otu_mini[,2:4], # use built-in hyperparameters
           "glmnet",
           outcome_colname = "Otu00001",
           find_feature_importance = TRUE,
           seed = 2019,
           kfold = 2,
           cv_times = 2,
           group = sample(LETTERS[1:10], nrow(otu_mini), replace = TRUE)
    ),
    otu_mini_cont_results1
  )
})

test_that("run_ml works for random forest", {
  expect_equal_ml_results( # use built-in hyperparams function
    mikropml::run_ml(otu_mini,
      "rf",
      outcome_colname = "dx",
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 5
    ),
    otu_mini_results2,
    tol = 1e-3
  )
})
test_that("run_ml works for svmRadial", {
  expect_equal_ml_results(
    mikropml::run_ml(otu_mini,
      "svmRadial",
      outcome_colname = "dx",
      hyperparameters = test_hyperparams %>% get_hyperparams_from_df("svmRadial"),
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 5
    ),
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
      cv_times = 5
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
      hyperparameters = test_hyperparams %>% get_hyperparams_from_df("rpart2"),
      find_feature_importance = FALSE,
      seed = 2019,
      kfold = 2,
      cv_times = 5
    ),
    otu_mini_results5
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


