# Get feature importance using the permutation method

Calculates feature importance using a trained model and test data.
Requires the `future.apply` package.

## Usage

``` r
get_feature_importance(
  trained_model,
  test_data,
  outcome_colname,
  perf_metric_function,
  perf_metric_name,
  class_probs,
  method,
  seed = NA,
  corr_thresh = 1,
  groups = NULL,
  nperms = 100,
  corr_method = "spearman"
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

- corr_thresh:

  For feature importance, group correlations above or equal to
  `corr_thresh` (range `0` to `1`; default: `1`).

- groups:

  Vector of feature names to group together during permutation. Each
  element should be a string with feature names separated by a pipe
  character (`|`). If this is `NULL` (default), correlated features will
  be grouped together based on `corr_thresh`.

- nperms:

  number of permutations to perform (default: `100`).

- corr_method:

  Correlation method. Options are the same as those supported by
  [`stats::cor`](https://rdrr.io/r/stats/cor.html): spearman, pearson,
  kendall. (default: spearman)

## Value

Data frame with performance metrics for when each feature (or group of
correlated features; `feat`) is permuted (`perf_metric`), differences
between the actual test performance metric on and the permuted
performance metric (`perf_metric_diff`; test minus permuted
performance), and the p-value (`pvalue`: the probability of obtaining
the actual performance value under the null hypothesis). Features with a
larger `perf_metric_diff` are more important. The performance metric
name (`perf_metric_name`) and seed (`seed`) are also returned.

## Details

For permutation tests, the p-value is the number of permutation
statistics that are greater than the test statistic, divided by the
number of permutations. In our case, the permutation statistic is the
model performance (e.g. AUROC) after randomizing the order of
observations for one feature, and the test statistic is the actual
performance on the test data. By default we perform 100 permutations per
feature; increasing this will increase the precision of estimating the
null distribution, but also increases runtime. The p-value represents
the probability of obtaining the actual performance in the event that
the null hypothesis is true, where the null hypothesis is that the
feature is not important for model performance.

We strongly recommend providing multiple cores to speed up computation
time. See [our vignette on parallel
processing](http://www.schlosslab.org/mikropml/articles/parallel.md) for
more details.

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
# If you called `run_ml()` with `feature_importance = FALSE` (the default),
# you can use `get_feature_importance()` later as long as you have the
# trained model and test data.
results <- run_ml(otu_small, "glmnet", kfold = 2, cv_times = 2)
names(results$trained_model$trainingData)[1] <- "dx"
feat_imp <- get_feature_importance(results$trained_model,
  results$trained_model$trainingData,
  results$test_data,
  "dx",
  multiClassSummary,
  "AUC",
  class_probs = TRUE,
  method = "glmnet"
)

# We strongly recommend providing multiple cores to speed up computation time.
# Do this before calling `get_feature_importance()`.
doFuture::registerDoFuture()
future::plan(future::multicore, workers = 2)

# Optionally, you can group features together with a custom grouping
feat_imp <- get_feature_importance(results$trained_model,
  results$trained_model$trainingData,
  results$test_data,
  "dx",
  multiClassSummary,
  "AUC",
  class_probs = TRUE,
  method = "glmnet",
  groups = c(
    "Otu00007", "Otu00008", "Otu00009", "Otu00011", "Otu00012",
    "Otu00015", "Otu00016", "Otu00018", "Otu00019", "Otu00020", "Otu00022",
    "Otu00023", "Otu00025", "Otu00028", "Otu00029", "Otu00030", "Otu00035",
    "Otu00036", "Otu00037", "Otu00038", "Otu00039", "Otu00040", "Otu00047",
    "Otu00050", "Otu00052", "Otu00054", "Otu00055", "Otu00056", "Otu00060",
    "Otu00003|Otu00002|Otu00005|Otu00024|Otu00032|Otu00041|Otu00053",
    "Otu00014|Otu00021|Otu00017|Otu00031|Otu00057",
    "Otu00013|Otu00006", "Otu00026|Otu00001|Otu00034|Otu00048",
    "Otu00033|Otu00010",
    "Otu00042|Otu00004", "Otu00043|Otu00027|Otu00049", "Otu00051|Otu00045",
    "Otu00058|Otu00044", "Otu00059|Otu00046"
  )
)

# the function can show a progress bar if you have the `progressr` package installed.
## optionally, specify the progress bar format:
progressr::handlers(progressr::handler_progress(
  format = ":message :bar :percent | elapsed: :elapsed | eta: :eta",
  clear = FALSE,
  show_after = 0
))
## tell progressr to always report progress
progressr::handlers(global = TRUE)
## run the function and watch the live progress udpates
feat_imp <- get_feature_importance(results$trained_model,
  results$trained_model$trainingData,
  results$test_data,
  "dx",
  multiClassSummary,
  "AUC",
  class_probs = TRUE,
  method = "glmnet"
)

# You can specify any correlation method supported by `stats::cor`:
feat_imp <- get_feature_importance(results$trained_model,
  results$trained_model$trainingData,
  results$test_data,
  "dx",
  multiClassSummary,
  "AUC",
  class_probs = TRUE,
  method = "glmnet",
  corr_method = "pearson"
)
} # }
```
