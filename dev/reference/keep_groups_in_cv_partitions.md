# Whether groups can be kept together in partitions during cross-validation

Whether groups can be kept together in partitions during
cross-validation

## Usage

``` r
keep_groups_in_cv_partitions(groups, group_partitions, kfold)
```

## Arguments

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

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

## Value

`TRUE` if possible, `FALSE` otherwise

## Author

Kelly Sovacool, <sovacool@umich.edu>
