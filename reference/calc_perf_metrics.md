# Get performance metrics for test data

Get performance metrics for test data

## Usage

``` r
calc_perf_metrics(
  test_data,
  trained_model,
  outcome_colname,
  perf_metric_function,
  class_probs
)
```

## Arguments

- test_data:

  Held out test data: dataframe of outcome and features.

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

- class_probs:

  Whether to use class probabilities (TRUE for categorical outcomes,
  FALSE for numeric outcomes).

## Value

Dataframe of performance metrics.

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
calc_perf_metrics(results$test_data,
  results$trained_model,
  "dx",
  multiClassSummary,
  class_probs = TRUE
)
} # }
```
