# Get permuted performance metric difference for a single feature (or group of features)

Requires the `future.apply` package

## Usage

``` r
find_permuted_perf_metric(
  test_data,
  trained_model,
  outcome_colname,
  perf_metric_function,
  perf_metric_name,
  class_probs,
  feat,
  test_perf_value,
  nperms = 100,
  alpha = 0.05,
  progbar = NULL
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

- perf_metric_name:

  The column name from the output of the function provided to
  perf_metric_function that is to be used as the performance metric.
  Defaults: binary classification = `"ROC"`, multi-class classification
  = `"logLoss"`, regression = `"RMSE"`.

- class_probs:

  Whether to use class probabilities (TRUE for categorical outcomes,
  FALSE for numeric outcomes).

- feat:

  feature or group of correlated features to permute.

- test_perf_value:

  value of the true performance metric on the held-out test data.

- nperms:

  number of permutations to perform (default: `100`).

- alpha:

  alpha level for the confidence interval (default: `0.05` to obtain a
  95% confidence interval)

- progbar:

  optional progress bar (default: `NULL`)

## Value

vector of mean permuted performance and mean difference between test and
permuted performance (test minus permuted performance)

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>
