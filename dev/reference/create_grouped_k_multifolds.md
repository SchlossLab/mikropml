# Splitting into folds for cross-validation when using groups

Like
[createMultiFolds](https://rdrr.io/pkg/caret/man/createDataPartition.html)
but still splitting by groups using
[groupKFold](https://rdrr.io/pkg/caret/man/createDataPartition.html).
Code modified from
[createMultiFolds](https://rdrr.io/pkg/caret/man/createDataPartition.html).

## Usage

``` r
create_grouped_k_multifolds(groups, kfold = 10, cv_times = 5)
```

## Arguments

- groups:

  equivalent to y in caret::createMultiFolds

- kfold:

  equivalent to k in caret::createMultiFolds

- cv_times:

  equivalent to cv_times in caret::createMultiFolds

## Value

indices of folds for CV

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(0)
groups <- c("A", "B", "A", "B", "C", "C", "A", "A", "D")
folds <- create_grouped_k_multifolds(groups, kfold = 2, cv_times = 2)
} # }
```
