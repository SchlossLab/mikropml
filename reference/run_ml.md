# Run the machine learning pipeline

This function splits the data set into a train & test set, trains
machine learning (ML) models using k-fold cross-validation, evaluates
the best model on the held-out test set, and optionally calculates
feature importance using the framework outlined in Topçuoğlu *et al.*
2020 ([doi:10.1128/mBio.00434-20](https://doi.org/10.1128/mBio.00434-20)
). Required inputs are a data frame (must contain an outcome variable
and all other columns as features) and the ML method. See
[`vignette('introduction')`](http://www.schlosslab.org/mikropml/articles/introduction.md)
for more details.

## Usage

``` r
run_ml(dataset, ...)

# S4 method for class 'TreeSummarizedExperiment'
run_ml(
  dataset,
  method,
  outcome_colname,
  assay.type = "counts",
  col.var = NULL,
  altexp = NULL,
  ...
)

# S4 method for class 'ANY'
run_ml(
  dataset,
  method,
  outcome_colname = NULL,
  hyperparameters = NULL,
  find_feature_importance = FALSE,
  calculate_performance = TRUE,
  kfold = 5,
  cv_times = 100,
  cross_val = NULL,
  training_frac = 0.8,
  perf_metric_function = NULL,
  perf_metric_name = NULL,
  groups = NULL,
  group_partitions = NULL,
  corr_thresh = 1,
  seed = NA,
  ...
)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- ...:

  All additional arguments are passed on to
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html), such as
  case weights via the `weights` argument or `ntree` for `rf` models.
  See the [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html)
  docs for more details.

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- assay.type:

  The name of assay from `dataset` when the object is in
  `TreeSummarizedExperiment` format. This assay is used as an input.

- col.var:

  The name of sample matdata variables from `colData` slot of `dataset`
  when the object is in `TreeSummarizedExperiment` format. These
  variables are used as predictors.

- altexp:

  The name of alternative experiment (`altExp`) from `dataset` when the
  object is in `TreeSummarizedExperiment` format. This can be used to
  select an experiment for the input.

- hyperparameters:

  Dataframe of hyperparameters (default `NULL`; sensible defaults will
  be chosen automatically).

- find_feature_importance:

  Run permutation importance (default: `FALSE`). `TRUE` is recommended
  if you would like to identify features important for predicting your
  outcome, but it is resource-intensive.

- calculate_performance:

  Whether to calculate performance metrics (default: `TRUE`). You might
  choose to skip this if you do not perform cross-validation during
  model training.

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

- cv_times:

  Number of cross-validation partitions to create (default: `100`).

- cross_val:

  a custom cross-validation scheme from
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
  (default: `NULL`, uses `kfold` cross validation repeated `cv_times`).
  `kfold` and `cv_times` are ignored if the user provides a custom
  cross-validation scheme. See the
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
  docs for information on how to use it.

- training_frac:

  Fraction of data for training set (default: `0.8`). Rows from the
  dataset will be randomly selected for the training set, and all
  remaining rows will be used in the testing set. Alternatively, if you
  provide a vector of integers, these will be used as the row indices
  for the training set. All remaining rows will be used in the testing
  set.

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

- groups:

  Vector of groups to keep together when splitting the data into train
  and test sets. If the number of groups in the training set is larger
  than `kfold`, the groups will also be kept together for
  cross-validation. Length matches the number of rows in the dataset
  (default: `NULL`).

- group_partitions:

  Specify how to assign `groups` to the training and testing partitions
  (default: `NULL`). If `groups` specifies that some samples belong to
  group `"A"` and some belong to group `"B"`, then setting
  `group_partitions = list(train = c("A", "B"), test = c("B"))` will
  result in all samples from group `"A"` being placed in the training
  set, some samples from `"B"` also in the training set, and the
  remaining samples from `"B"` in the testing set. The partition sizes
  will be as close to `training_frac` as possible. If the number of
  groups in the training set is larger than `kfold`, the groups will
  also be kept together for cross-validation.

- corr_thresh:

  For feature importance, group correlations above or equal to
  `corr_thresh` (range `0` to `1`; default: `1`).

- seed:

  Random seed (default: `NA`). Your results will only be reproducible if
  you set a seed.

## Value

Named list with results:

- `trained_model`: Output of
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html),
  including the best model.

- `test_data`: Part of the data that was used for testing.

- `performance`: Data frame of performance metrics. The first column is
  the cross-validation performance metric, and the last two columns are
  the ML method used and the seed (if one was set), respectively. All
  other columns are performance metrics calculated on the test data.
  This contains only one row, so you can easily combine performance data
  frames from multiple calls to `run_ml()` (see
  [`vignette("parallel")`](http://www.schlosslab.org/mikropml/articles/parallel.md)).

- `feature_importance`: If feature importances were calculated, a data
  frame where each row is a feature or correlated group. The columns are
  the performance metric of the permuted data, the difference between
  the true performance metric and the performance metric of the permuted
  data (true - permuted), the feature name, the ML method, the
  performance metric name, and the seed (if provided). For AUC and RMSE,
  the higher perf_metric_diff is, the more important that feature is for
  predicting the outcome. For log loss, the lower perf_metric_diff is,
  the more important that feature is for predicting the outcome.

## More details

For more details, please see [the
vignettes](http://www.schlosslab.org/mikropml/articles/).

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{

# regression
run_ml(otu_small, "glmnet",
  seed = 2019
)

# random forest w/ feature importance
run_ml(otu_small, "rf",
  outcome_colname = "dx",
  find_feature_importance = TRUE
)

# custom cross validation & hyperparameters
run_ml(otu_mini_bin[, 2:11],
  "glmnet",
  outcome_colname = "Otu00001",
  seed = 2019,
  hyperparameters = list(lambda = c(1e-04), alpha = 0),
  cross_val = caret::trainControl(method = "none"),
  calculate_performance = FALSE
)

# Create TreeSE dataset
library(TreeSummarizedExperiment)
df <- mikropml::otu_small
assay <- df[, !colnames(df) %in% c("dx"), drop = FALSE] |> t() |> as.matrix()
tse <- TreeSummarizedExperiment(assays = SimpleList(counts = assay))
colData(tse)[["dx"]] <- otu_mini_multi[["dx"]]

# Train model
res <- run_ml(
  tse,
  assay.type = "counts",
  method = "rf",
  outcome_colname = "dx"
)
} # }
```
