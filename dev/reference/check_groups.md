# Check grouping vector

Check grouping vector

## Usage

``` r
check_groups(dataset, groups, kfold)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- groups:

  Vector of groups to keep together when splitting the data into train
  and test sets. If the number of groups in the training set is larger
  than `kfold`, the groups will also be kept together for
  cross-validation. Length matches the number of rows in the dataset
  (default: `NULL`).

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_groups(mikropml::otu_mini_bin,
  sample(LETTERS, nrow(mikropml::otu_mini_bin), replace = TRUE),
  kfold = 2
)
} # }
```
