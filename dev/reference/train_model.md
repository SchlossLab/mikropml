# Train model using [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

Train model using
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

## Usage

``` r
train_model(
  train_data,
  outcome_colname,
  method,
  cv,
  perf_metric_name,
  tune_grid,
  ...
)
```

## Arguments

- train_data:

  Training data. Expected to be a subset of the full dataset.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

- cv:

  Cross-validation caret scheme from
  [`define_cv()`](http://www.schlosslab.org/mikropml/dev/reference/define_cv.md).

- perf_metric_name:

  The column name from the output of the function provided to
  perf_metric_function that is to be used as the performance metric.
  Defaults: binary classification = `"ROC"`, multi-class classification
  = `"logLoss"`, regression = `"RMSE"`.

- tune_grid:

  Tuning grid from
  [`get_tuning_grid()`](http://www.schlosslab.org/mikropml/dev/reference/get_tuning_grid.md).#'

- ...:

  All additional arguments are passed on to
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html), such as
  case weights via the `weights` argument or `ntree` for `rf` models.
  See the [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html)
  docs for more details.

## Value

Trained model from
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
training_data <- otu_mini_bin_results_glmnet$trained_model$trainingData %>%
  dplyr::rename(dx = .outcome)
method <- "rf"
hyperparameters <- get_hyperparams_list(otu_mini_bin, method)
cross_val <- define_cv(training_data,
  "dx",
  hyperparameters,
  perf_metric_function = caret::multiClassSummary,
  class_probs = TRUE,
  cv_times = 2
)
tune_grid <- get_tuning_grid(hyperparameters, method)

rf_model <- train_model(
  training_data,
  "dx",
  method,
  cross_val,
  "AUC",
  tune_grid,
  ntree = 1000
)
rf_model$results %>% dplyr::select(mtry, AUC, prAUC)
} # }
```
