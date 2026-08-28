# Get model performance metrics as a one-row tibble

Get model performance metrics as a one-row tibble

## Usage

``` r
get_performance_tbl(
  trained_model,
  test_data,
  outcome_colname,
  perf_metric_function,
  perf_metric_name,
  class_probs,
  method,
  seed = NA
)
```

## Arguments

- trained_model:

  Trained model from
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

- test_data:

  Held out test data: dataframe of outcome and features.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- perf_metric_function:

  Function to calculate the performance metric to be used for
  cross-validation and test performance. Some functions are provided by
  caret (see
  [`caret::defaultSummary()`](https://rdrr.io/pkg/caret/man/postResample.html)).
  Defaults: binary classification = `twoClassSummary`, multi-class
  classification = `multiClassSummary`, regression = `defaultSummary`.

- perf_metric_name:

  The column name from the output of the function provided to
  perf_metric_function that is to be used as the performance metric.
  Defaults: binary classification = `"ROC"`, multi-class classification
  = `"logLoss"`, regression = `"RMSE"`.

- class_probs:

  Whether to use class probabilities (TRUE for categorical outcomes,
  FALSE for numeric outcomes).

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

- seed:

  Random seed (default: `NA`). Your results will only be reproducible if
  you set a seed.

## Value

A one-row tibble with a column for the cross-validation performance,
columns for each of the performance metrics for the test data, plus the
`method`, and `seed`.

## Author

Kelly Sovacool, <sovacool@umich.edu>

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
names(results$trained_model$trainingData)[1] <- "dx"
get_performance_tbl(results$trained_model, results$test_data,
  "dx",
  multiClassSummary, "AUC",
  class_probs = TRUE,
  method = "glmnet"
)
} # }
```
