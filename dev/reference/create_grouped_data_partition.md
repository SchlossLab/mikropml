# Split into train and test set while splitting by groups. When `group_partitions` is `NULL`, all samples from each group will go into either the training set or the testing set. Otherwise, the groups will be split according to `group_partitions`

Split into train and test set while splitting by groups. When
`group_partitions` is `NULL`, all samples from each group will go into
either the training set or the testing set. Otherwise, the groups will
be split according to `group_partitions`

## Usage

``` r
create_grouped_data_partition(
  groups,
  group_partitions = NULL,
  training_frac = 0.8
)
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

- training_frac:

  Fraction of data for training set (default: `0.8`). Rows from the
  dataset will be randomly selected for the training set, and all
  remaining rows will be used in the testing set. Alternatively, if you
  provide a vector of integers, these will be used as the row indices
  for the training set. All remaining rows will be used in the testing
  set.

## Value

vector of row indices for the training set

## Author

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
set.seed(0)
create_grouped_data_partition(groups, training_frac = 0.8)
groups <- rep.int(c("A", "B", "C"), 3)
create_grouped_data_partition(groups,
  group_partitions = list(train = c("A"), test = c("A", "B", "C"))
)
} # }
```
