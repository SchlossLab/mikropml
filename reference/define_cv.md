# Define cross-validation scheme and training parameters

Define cross-validation scheme and training parameters

## Usage

``` r
define_cv(
  train_data,
  outcome_colname,
  hyperparams_list,
  perf_metric_function,
  class_probs,
  kfold = 5,
  cv_times = 100,
  groups = NULL,
  group_partitions = NULL
)
```

## Arguments

- train_data:

  Dataframe for training model.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- hyperparams_list:

  Named list of lists of hyperparameters.

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

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

- cv_times:

  Number of cross-validation partitions to create (default: `100`).

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

## Value

Caret object for trainControl that controls cross-validation

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
training_inds <- get_partition_indices(otu_small %>% dplyr::pull("dx"),
  training_frac = 0.8,
  groups = NULL
)
train_data <- otu_small[training_inds, ]
test_data <- otu_small[-training_inds, ]
cv <- define_cv(train_data,
  outcome_colname = "dx",
  hyperparams_list = get_hyperparams_list(otu_small, "glmnet"),
  perf_metric_function = caret::multiClassSummary,
  class_probs = TRUE,
  kfold = 5
)
```
