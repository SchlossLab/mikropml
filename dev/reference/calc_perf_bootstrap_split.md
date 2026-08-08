# Calculate performance for a single split from [`rsample::bootstraps()`](https://rsample.tidymodels.org/reference/bootstraps.html)

Used by
[`bootstrap_performance()`](http://www.schlosslab.org/mikropml/dev/reference/bootstrap_performance.md).

## Usage

``` r
calc_perf_bootstrap_split(
  test_data_split,
  trained_model,
  outcome_colname,
  perf_metric_function,
  perf_metric_name,
  class_probs,
  method,
  seed
)
```

## Arguments

- test_data_split:

  a single bootstrap of the test set from
  [`rsample::bootstraps()`](https://rsample.tidymodels.org/reference/bootstraps.html)

- trained_model:

  Trained model from
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

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

a long data frame of performance metrics for
[`rsample::int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)

## Author

Kelly Sovacool, <sovacool@umich.edu>
