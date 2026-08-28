# Select indices to partition the data into training & testing sets.

Use this function to get the row indices for the training set.

## Usage

``` r
get_partition_indices(
  outcomes,
  training_frac = 0.8,
  groups = NULL,
  group_partitions = NULL
)
```

## Arguments

- outcomes:

  vector of outcomes

- training_frac:

  Fraction of data for training set (default: `0.8`). Rows from the
  dataset will be randomly selected for the training set, and all
  remaining rows will be used in the testing set. Alternatively, if you
  provide a vector of integers, these will be used as the row indices
  for the training set. All remaining rows will be used in the testing
  set.

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

Vector of row indices for the training set.

## Details

If `groups` is `NULL`, uses
[createDataPartition](https://rdrr.io/pkg/caret/man/createDataPartition.html).
Otherwise, uses
[`create_grouped_data_partition()`](http://www.schlosslab.org/mikropml/reference/create_grouped_data_partition.md).

Set the seed prior to calling this function if you would like your data
partitions to be reproducible (recommended).

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
training_inds <- get_partition_indices(otu_mini_bin$dx)
train_data <- otu_mini_bin[training_inds, ]
test_data <- otu_mini_bin[-training_inds, ]
```
